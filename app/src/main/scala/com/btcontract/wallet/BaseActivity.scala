package com.btcontract.wallet

import android.content.pm.PackageManager
import android.content.{DialogInterface, Intent}
import android.graphics.Bitmap.Config.ARGB_8888
import android.graphics.{Bitmap, Color}
import android.net.Uri
import android.os.Bundle
import android.text.method.LinkMovementMethod
import android.text.{Editable, Spanned, TextWatcher}
import android.view.View.OnClickListener
import android.view.{View, ViewGroup, WindowManager}
import android.widget._
import androidx.appcompat.app.{AlertDialog, AppCompatActivity}
import androidx.appcompat.widget.AppCompatButton
import androidx.cardview.widget.CardView
import androidx.core.app.ActivityCompat
import androidx.core.content.{ContextCompat, FileProvider}
import androidx.recyclerview.widget.RecyclerView
import com.btcontract.wallet.BaseActivity.StringOps
import com.btcontract.wallet.Colors._
import com.btcontract.wallet.R.string._
import com.btcontract.wallet.utils.{BitcoinUri, InputParser}
import com.cottacush.android.currencyedittext.CurrencyEditText
import com.google.android.material.slider.Slider
import com.google.android.material.snackbar.{BaseTransientBottomBar, Snackbar}
import com.google.android.material.textfield.TextInputLayout
import com.google.zxing.qrcode.QRCodeWriter
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import com.ornach.nobobutton.NoboButton
import fr.acinq.bitcoin.DeterministicWallet.ExtendedPublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.fee.{FeeratePerByte, FeeratePerKw}
import immortan.crypto.Tools._
import immortan.utils._
import org.apmem.tools.layouts.FlowLayout

import java.io.{File, FileOutputStream}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}


object BaseActivity {
  implicit class StringOps(source: String) {
    def html: Spanned = android.text.Html.fromHtml(source)
    def humanFour: String = "<tt>" + source.grouped(4).mkString(s"\u0020") + "</tt>"

    def short: String = {
      val len = source.length
      val firstFirst = source.slice(0, 4)
      val secondFirst = source.slice(4, 8)
      val firstLast = source.slice(len - 8, len - 4)
      val secondLast = source.slice(len - 4, len)

      val doubleSmall = "<sup><small><small>&#8230;</small></small></sup>"
      s"<tt>$firstFirst&#160;$secondFirst&#160;$doubleSmall&#160;$firstLast&#160;$secondLast</tt>"
    }
  }
}

object ClassNames {
  val qrChainActivityClass: Class[QRChainActivity] = classOf[QRChainActivity]
  val settingsActivityClass: Class[SettingsActivity] = classOf[SettingsActivity]
  val hubActivityClass: Class[HubActivity] = classOf[HubActivity]
}

object Colors {
  val cardIn: String = "#" + WalletApp.app.getResources.getString(R.color.colorAccent).substring(3)
  val cardOut: String = "#" + WalletApp.app.getResources.getString(R.color.cardOutText).substring(3)
  val cardZero: String = "#" + WalletApp.app.getResources.getString(R.color.cardZeroText).substring(3)
  val signCardZero: String = "#" + WalletApp.app.getResources.getString(R.color.signCardZeroText).substring(3)
}

trait ExternalDataChecker {
  def checkExternalData(onNothing: Runnable): Unit
  val noneRunnable: Runnable = new Runnable {
    def run: Unit = none
  }
}

trait ChoiceReceiver {
  def onChoiceMade(tag: AnyRef, pos: Int): Unit
}

trait BaseActivity extends AppCompatActivity { me =>
  lazy val qrSize: Int = getResources.getDimensionPixelSize(R.dimen.qr_size)
  val nothingUsefulTask: Runnable = UITask(WalletApp.app quickToast error_nothing_useful)
  val timer: java.util.Timer = new java.util.Timer

  val exitTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    finish
  }

  val goTo: Class[_] => Any = target => {
    this startActivity new Intent(me, target)
    InputParser.DoNotEraseRecordedValue
  }

  def goToWithValue(target: Class[_], value: Any): Any = {
    // Utility method in case if target expects a value
    InputParser.value = value
    goTo(target)
  }

  def START(state: Bundle): Unit

  override def onCreate(savedActivityState: Bundle): Unit = {
    Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedActivityState)
    START(savedActivityState)
  }

  override def onDestroy: Unit = {
    super.onDestroy
    timer.cancel
  }

  // Helpers

  def titleViewFromUri(uri: BitcoinUri): TitleView = {
    val label = uri.label.map(label => s"<br><br><b>$label</b>").getOrElse(new String)
    val message = uri.message.map(message => s"<br><i>$message<i>").getOrElse(new String)
    val caption = getString(dialog_send_btc).format(uri.address.short, label + message)
    val title = new TitleView(caption)

    for (amount <- uri.amount) {
      val amountHuman = WalletApp.denom.parsedWithSignTT(amount, cardIn, signCardZero)
      val requested = getString(dialog_requested).format(amountHuman)
      addFlowChip(title.flow, requested, R.drawable.border_gray)
    }

    title
  }

  def browse(maybeUri: String): Unit = try {
    me startActivity new Intent(Intent.ACTION_VIEW, Uri parse maybeUri)
  } catch { case exception: Throwable => me onFail exception }

  def share(text: CharSequence): Unit = startActivity {
    val shareAction = (new Intent).setAction(Intent.ACTION_SEND)
    shareAction.setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)
  }

  def viewRecoveryCode: Unit = {
    val content = new TitleView(me getString settings_view_revocery_phrase_ext)
    getWindow.setFlags(WindowManager.LayoutParams.FLAG_SECURE, WindowManager.LayoutParams.FLAG_SECURE)
    new AlertDialog.Builder(me).setView(content.view).show setOnDismissListener new DialogInterface.OnDismissListener {
      override def onDismiss(dialog: DialogInterface): Unit = getWindow.clearFlags(WindowManager.LayoutParams.FLAG_SECURE)
    }

    for (mnemonicWord \ mnemonicIndex <- WalletApp.secret.mnemonic.zipWithIndex) {
      val oneWord = s"<font color=$cardZero>${mnemonicIndex + 1}</font> $mnemonicWord"
      addFlowChip(content.flow, oneWord, R.drawable.border_green)
    }
  }

  // Snackbar

  def snack(parent: View, msg: CharSequence, res: Int): Try[Snackbar] = Try {
    val snack: Snackbar = Snackbar.make(parent, msg, BaseTransientBottomBar.LENGTH_INDEFINITE)
    snack.getView.findViewById(com.google.android.material.R.id.snackbar_text).asInstanceOf[TextView].setMaxLines(5)
    snack
  }

  def snack(parent: View, msg: CharSequence, res: Int, fun: Snackbar => Unit): Try[Snackbar] = snack(parent, msg, res) map { snack =>
    val listener = onButtonTap(fun apply snack)
    snack.setAction(res, listener).show
    snack
  }

  // Listener helpers

  def onButtonTap(fun: => Unit): OnClickListener = new OnClickListener {
    def onClick(view: View): Unit = fun
  }

  def onTextChange(fun: String => Unit): TextWatcher = new TextWatcher {
    override def onTextChanged(c: CharSequence, x: Int, y: Int, z: Int): Unit = fun(c.toString)
    override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int): Unit = none
    override def afterTextChanged(e: Editable): Unit = none
  }

  def runInFutureProcessOnUI[T](fun: => T, no: Throwable => Unit)(ok: T => Unit): Unit = runFutureProcessOnUI[T](Future(fun), no)(ok)

  def runFutureProcessOnUI[T](fun: Future[T], no: Throwable => Unit)(ok: T => Unit): Unit = fun onComplete {
    case Success(result) => UITask(ok apply result).run
    case Failure(error) => UITask(no apply error).run
  }

  def setVis(isVisible: Boolean, view: View): Unit = {
    val nextMode = if (isVisible) View.VISIBLE else View.GONE
    if (view.getVisibility != nextMode) view.setVisibility(nextMode)
  }

  def setVisMany(items: (Boolean, View)*): Unit = {
    for (isVisible \ view <- items) setVis(isVisible, view)
  }

  def UITask(fun: => Any): java.util.TimerTask = {
    val runnableExec = new Runnable { override def run: Unit = fun }
    new java.util.TimerTask { def run: Unit = me runOnUiThread runnableExec }
  }

  // Builders

  def clickableTextField(view: View): TextView = {
    val field: TextView = view.asInstanceOf[TextView]
    field setMovementMethod LinkMovementMethod.getInstance
    field
  }

  def titleBodyAsViewBuilder(title: View, body: View): AlertDialog.Builder =
    new AlertDialog.Builder(me).setCustomTitle(title).setView(body)

  def onFail(error: String): Unit = UITask {
    val bld = titleBodyAsViewBuilder(null, error.asDefView)
    val bld1 = bld.setPositiveButton(dialog_ok, null)
    showForm(bld1.create)
  }.run

  def onFail(error: Throwable): Unit = error match {
    case exc if exc.getCause.isInstanceOf[java.io.InterruptedIOException] =>
    case _ => onFail(error.toString)
  }

  def getPositiveButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_POSITIVE)

  def getNegativeButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEGATIVE)

  def getNeutralButton(alert: AlertDialog): Button = alert.getButton(DialogInterface.BUTTON_NEUTRAL)

  def mkCheckForm(ok: AlertDialog => Unit, no: => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int): AlertDialog = {
    // Create alert dialog where NEGATIVE button removes a dialog AND calls a respected provided function
    // both POSITIVE and NEGATIVE buttons may be omitted by providing -1 as their resource ids
    if (-1 != noRes) bld.setNegativeButton(noRes, null)
    if (-1 != okRes) bld.setPositiveButton(okRes, null)
    val alert = showForm(bld.create)

    val posAct = onButtonTap {
      ok(alert)
    }

    val negAct = onButtonTap {
      alert.dismiss
      no
    }

    if (-1 != noRes) getNegativeButton(alert) setOnClickListener negAct
    if (-1 != okRes) getPositiveButton(alert) setOnClickListener posAct
    alert
  }

  def mkCheckFormNeutral(ok: AlertDialog => Unit, no: => Unit, neutral: AlertDialog => Unit, bld: AlertDialog.Builder, okRes: Int, noRes: Int, neutralRes: Int): AlertDialog = {

    if (-1 != neutralRes) bld.setNeutralButton(neutralRes, null)
    val alert = mkCheckForm(ok, no, bld, okRes, noRes)

    val neutralAct = onButtonTap {
      neutral(alert)
    }

    // Extend base dialog with a special NEUTRAL button, may be omitted by providing -1
    if (-1 != neutralRes) getNeutralButton(alert) setOnClickListener neutralAct
    alert
  }

  def showForm(alertDialog: AlertDialog): AlertDialog = {
    // Display dialog taking into account that host activity may not be there
    // Popup window is not cancellable on touch outside and may be width-limited

    try {
      alertDialog.show
      // First, limit popup window width in case if this is a tablet, then make sure it does not blow up if called on destroyed activity
      if (WalletApp.app.scrWidth > 2.3) alertDialog.getWindow.setLayout(WalletApp.app.maxDialog.toInt, ViewGroup.LayoutParams.WRAP_CONTENT)
      clickableTextField(alertDialog findViewById android.R.id.message)
      alertDialog.setCanceledOnTouchOutside(false)
    } catch none
    alertDialog
  }

  // Scanner

  final val scannerRequestCode = 101

  def callScanner(sheet: sheets.ScannerBottomSheet): Unit = {
    val allowed = ContextCompat.checkSelfPermission(me, android.Manifest.permission.CAMERA) == PackageManager.PERMISSION_GRANTED
    if (!allowed) ActivityCompat.requestPermissions(me, Array(android.Manifest.permission.CAMERA), scannerRequestCode)
    else sheet.show(getSupportFragmentManager, "scanner-bottom-sheet-fragment")
  }

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int, shareText: Option[String] = None): TextView = {
    def copyText(defText: String): Unit = WalletApp.app.copy(shareText getOrElse defText)
    addFlowChip(flow, chipText, backgroundRes, copyText _)
  }

  def addFlowChip(flow: FlowLayout, chipText: String, backgroundRes: Int, onTap: String => Unit): TextView = {
    val text = getLayoutInflater.inflate(R.layout.frag_chip_text, flow, false).asInstanceOf[TextView]
    text setOnClickListener onButtonTap(onTap apply text.getText.toString)
    text setBackgroundResource backgroundRes
    text setText chipText.html

    flow setVisibility View.VISIBLE
    flow addView text
    text
  }

  def showKeys(input: EditText): Unit = {
    // Popup forms can't show keyboard immediately due to animation, so delay it a bit
    def process: Unit = runAnd(input.requestFocus)(WalletApp.app showKeys input)
    timer.schedule(UITask(process), 225)
  }

  def singleInputPopup: (View, TextInputLayout, EditText, CheckBox, TextView) = {
    val container = getLayoutInflater.inflate(R.layout.frag_hint_input, null, false)
    val extraOption = container.findViewById(R.id.extraOption).asInstanceOf[CheckBox]
    val extraOptionText = container.findViewById(R.id.extraOptionText).asInstanceOf[TextView]
    val extraInputLayout = container.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput = container.findViewById(R.id.extraInput).asInstanceOf[EditText]
    (container, extraInputLayout, extraInput, extraOption, extraOptionText)
  }

  // Rich popup title

  implicit class TitleView(titleText: String) {
    val view: LinearLayout = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val flow: FlowLayout = view.findViewById(R.id.tipExtraTags).asInstanceOf[FlowLayout]
    val tipTitle: TextView = clickableTextField(view findViewById R.id.tipTitle)
    tipTitle.setText(titleText.html)

    def asDefView: LinearLayout = {
      view.setBackgroundColor(0x22AAAAAA)
      view
    }

    def asColoredView(colorRes: Int): LinearLayout = {
      val resultColor = ContextCompat.getColor(me, colorRes)
      view.setBackgroundColor(resultColor)
      view
    }
  }

  // Fiat / BTC converter

  def updatePopupButton(button: Button, isEnabled: Boolean): Unit = {
    val alpha = if (isEnabled) 1F else 0.3F
    button.setEnabled(isEnabled)
    button.setAlpha(alpha)
  }

  class RateManager(val content: ViewGroup, extraText: Option[String], visHintRes: Int, rates: Fiat2Btc, fiatCode: String) {
    val fiatInputAmount: CurrencyEditText = content.findViewById(R.id.fiatInputAmount).asInstanceOf[CurrencyEditText]
    val inputAmount: CurrencyEditText = content.findViewById(R.id.inputAmount).asInstanceOf[CurrencyEditText]

    val fiatInputAmountHint: TextView = content.findViewById(R.id.fiatInputAmountHint).asInstanceOf[TextView]
    val inputAmountHint: TextView = content.findViewById(R.id.inputAmountHint).asInstanceOf[TextView]

    val hintFiatDenom: TextView = clickableTextField(content findViewById R.id.hintFiatDenom)
    val hintDenom: TextView = clickableTextField(content findViewById R.id.hintDenom)

    val extraInputOption: TextView = content.findViewById(R.id.extraInputOption).asInstanceOf[TextView]
    val extraInputVisibility: TextView = content.findViewById(R.id.extraInputVisibility).asInstanceOf[TextView]
    val extraInputLayout: TextInputLayout = content.findViewById(R.id.extraInputLayout).asInstanceOf[TextInputLayout]
    val extraInput: EditText = content.findViewById(R.id.extraInput).asInstanceOf[EditText]

    def updateText(value: MilliSatoshi): Unit = {
      val amount = WalletApp.denom.fromMsat(value).toString
      runAnd(inputAmount.requestFocus)(inputAmount setText amount)
      updateFiatInput
    }

    def bigDecimalFrom(input: CurrencyEditText): BigDecimal = BigDecimal(input.getNumericValueBigDecimal)
    def resultExtraInput: Option[String] = Option(extraInput.getText.toString).map(trimmed).filter(_.nonEmpty)
    def resultMsat: MilliSatoshi = (bigDecimalFrom(inputAmount) * WalletApp.denom.factor).toLong.msat

    def updatedFiatFromBtc: String =
      WalletApp.msatInFiat(rates, fiatCode)(resultMsat)
        .filter(0.001D <= _).map(_.toString)
        .getOrElse("0.00")

    def updatedBtcFromFiat: String =
      WalletApp.currentRate(rates, fiatCode)
        .map(perBtc => bigDecimalFrom(fiatInputAmount) / perBtc)
        .filter(0.000000001D <= _).map(Denomination.btcBigDecimal2MSat)
        .map(WalletApp.denom.fromMsat).map(_.toString)
        .getOrElse("0.00")

    def updateFiatInput: Unit = {
      fiatInputAmount setText updatedFiatFromBtc
      fiatInputAmount setMaxNumberOfDecimalDigits 2
    }

    def updateBtcInput: Unit = {
      inputAmount setText updatedBtcFromFiat
      inputAmount setMaxNumberOfDecimalDigits 8
    }

    // At first we set all extra input elements to zero, next we'll see if we should change this
    setVisMany(false -> extraInputLayout, false -> extraInputOption, false -> extraInputVisibility)

    for (hintText <- extraText) {
      val revealExtraInputListener = onButtonTap {
        setVisMany(true -> extraInputLayout, false -> extraInputOption)
        showKeys(extraInput)
      }

      extraInputLayout.setHint(hintText)
      extraInputOption.setText(hintText)
      extraInputVisibility.setText(visHintRes)
      extraInputOption.setOnClickListener(revealExtraInputListener)
      extraInputVisibility.setOnClickListener(revealExtraInputListener)
      setVisMany(true -> extraInputOption, true -> extraInputVisibility)
    }

    fiatInputAmount addTextChangedListener onTextChange { _ =>
      if (fiatInputAmount.hasFocus) updateBtcInput
    }

    inputAmount addTextChangedListener onTextChange { _ =>
      if (inputAmount.hasFocus) updateFiatInput
    }

    inputAmountHint setText WalletApp.denom.sign.toUpperCase
    fiatInputAmountHint setText fiatCode.toUpperCase
    fiatInputAmount setLocale Denomination.locale
    inputAmount setLocale Denomination.locale
  }

  class TwoSidedItem(val parent: View, firstText: CharSequence, secondText: CharSequence) {
    val secondItem: TextView = parent.findViewById(R.id.secondItem).asInstanceOf[TextView]
    val firstItem: TextView = parent.findViewById(R.id.firstItem).asInstanceOf[TextView]
    secondItem.setText(secondText)
    firstItem.setText(firstText)
  }

  class FeeView[T](from: FeeratePerByte, val content: View) {
    val feeRate: TextView = content.findViewById(R.id.feeRate).asInstanceOf[TextView]
    val txIssues: TextView = content.findViewById(R.id.txIssues).asInstanceOf[TextView]
    val bitcoinFee: TextView = content.findViewById(R.id.bitcoinFee).asInstanceOf[TextView]
    val fiatFee: TextView = content.findViewById(R.id.fiatFee).asInstanceOf[TextView]

    val customFeerate: Slider = content.findViewById(R.id.customFeerate).asInstanceOf[Slider]
    val customFeerateOption: TextView = content.findViewById(R.id.customFeerateOption).asInstanceOf[TextView]
    var worker: ThrottledWork[String, T] = _
    var rate: FeeratePerKw = _

    def update(feeOpt: Option[MilliSatoshi], showIssue: Boolean): Unit = {
      feeRate setText getString(dialog_fee_sat_vbyte).format(FeeratePerByte(rate).feerate.toLong).html
      setVisMany(feeOpt.isDefined -> bitcoinFee, feeOpt.isDefined -> fiatFee, showIssue -> txIssues)

      feeOpt.foreach { fee =>
        val humanFee = WalletApp.denom.parsedWithSign(fee, cardIn, cardZero).html
        fiatFee setText WalletApp.currentMsatInFiatHuman(fee).html
        bitcoinFee setText humanFee
      }
    }

    private val revealSlider = onButtonTap {
      val currentFeerate = FeeratePerByte(rate).feerate.toLong
      customFeerate.setValueFrom(from.feerate.toLong)
      customFeerate.setValueTo(currentFeerate * 10)
      customFeerate.setValue(currentFeerate)

      customFeerateOption setVisibility View.GONE
      customFeerate setVisibility View.VISIBLE
    }

    customFeerateOption.setOnClickListener(revealSlider)
    feeRate.setOnClickListener(revealSlider)

    customFeerate addOnChangeListener new Slider.OnChangeListener {
      override def onValueChange(slider: Slider, value: Float, fromUser: Boolean): Unit = {
        val feeratePerByte = FeeratePerByte(value.toLong.sat)
        rate = FeeratePerKw(feeratePerByte)
        worker addWork "SLIDER-CHANGE"
      }
    }
  }

  // Chan TX popup for wallets

  class ChainButtonsView(host: View) {
    val chainText: TextView = host.findViewById(R.id.chainText).asInstanceOf[TextView]
    val chainNextButton: NoboButton = host.findViewById(R.id.chainNextButton).asInstanceOf[NoboButton]
    val chainEditButton: NoboButton = host.findViewById(R.id.chainEditButton).asInstanceOf[NoboButton]
    val chainCancelButton: NoboButton = host.findViewById(R.id.chainCancelButton).asInstanceOf[NoboButton]
  }

  sealed trait HasHostView {
    val host: View
  }

  class ChainConfirmView(val host: View) extends HasHostView {
    val chainButtonsView: ChainButtonsView = new ChainButtonsView(host)
    val confirmFiat = new TwoSidedItem(host.findViewById(R.id.confirmFiat), getString(dialog_send_btc_confirm_fiat), new String)
    val confirmAmount = new TwoSidedItem(host.findViewById(R.id.confirmAmount), getString(dialog_send_btc_confirm_amount), new String)
    val confirmFee = new TwoSidedItem(host.findViewById(R.id.confirmFee), getString(dialog_send_btc_confirm_fee), new String)
  }

  class ChainEditView(val host: LinearLayout, specs: Seq[WalletSpec], manager: RateManager) extends HasHostView {
    val inputChain: LinearLayout = host.findViewById(R.id.inputChain).asInstanceOf[LinearLayout]

    val totalCanSend = specs.map(_.info.lastBalance).sum.toMilliSatoshi
    val canSend = WalletApp.denom.parsedWithSignTT(totalCanSend, cardIn, cardZero)
    val canSendFiat = WalletApp.currentMsatInFiatHuman(totalCanSend)

    manager.hintFiatDenom setText getString(dialog_up_to).format(canSendFiat).html
    manager.hintDenom setText getString(dialog_up_to).format(canSend).html
  }

  class CPFPView(val host: LinearLayout) extends HasHostView {
    val cpfpCurrent = new TwoSidedItem(host.findViewById(R.id.cpfpCurrent), getString(tx_cpfp_current), new String)
    val cpfpAfter = new TwoSidedItem(host.findViewById(R.id.cpfpAfter), getString(tx_cpfp_rbf_after), new String)
  }

  class RBFView(val host: LinearLayout) extends HasHostView {
    val rbfCurrent = new TwoSidedItem(host.findViewById(R.id.rbfCurrent), getString(tx_rbf_current), new String)
    val rbfIssue = host.findViewById(R.id.rbfIssue).asInstanceOf[TextView]
  }

  class ChainSendView(val specs: Seq[WalletSpec], badge: Option[String], visibilityRes: Int) { me =>
    val body: ScrollView = getLayoutInflater.inflate(R.layout.frag_input_on_chain, null).asInstanceOf[ScrollView]
    val manager: RateManager = new RateManager(body, badge, visibilityRes, WalletApp.fiatRates.info.rates, WalletApp.fiatCode)
    val chainEditView = new ChainEditView(body.findViewById(R.id.editChain).asInstanceOf[LinearLayout], specs, manager)
    lazy val chainConfirmView = new ChainConfirmView(body findViewById R.id.confirmChain)
    lazy val cpfpView = new CPFPView(body findViewById R.id.cpfp)
    lazy val rbfView = new RBFView(body findViewById R.id.rbf)
    var defaultView: HasHostView = chainEditView

    lazy private val views = List(chainEditView, cpfpView, rbfView, chainConfirmView)
    def switchTo(visibleSection: HasHostView): Unit = for (candidateSection <- views) setVis(isVisible = candidateSection == visibleSection, candidateSection.host)
    def switchButtons(alert: AlertDialog, on: Boolean): Unit = setVisMany(on -> getPositiveButton(alert), on -> getNegativeButton(alert), on -> getNeutralButton(alert), true -> body)

    def switchToDefault(alert: AlertDialog): Unit = {
      switchButtons(alert, on = true)
      switchTo(defaultView)
    }

    def switchToConfirm(alert: AlertDialog, response: ElectrumWallet.GenerateTxResponse): Unit = {
      chainConfirmView.chainButtonsView.chainCancelButton setOnClickListener onButtonTap(alert.dismiss)
      chainConfirmView.chainButtonsView.chainEditButton setOnClickListener onButtonTap(me switchToDefault alert)

      chainConfirmView.confirmFee.secondItem setText WalletApp.denom.parsedWithSignTT(response.fee.toMilliSatoshi, cardIn, cardZero).html
      chainConfirmView.confirmAmount.secondItem setText WalletApp.denom.parsedWithSignTT(response.transferred.toMilliSatoshi, cardIn, cardZero).html
      chainConfirmView.confirmFiat.secondItem setText WalletApp.currentMsatInFiatHuman(response.transferred.toMilliSatoshi).html

      switchButtons(alert, on = false)
      switchTo(chainConfirmView)
    }
  }

  abstract class WalletSelector(title: TitleView) {
    val info = addFlowChip(title.flow, getString(select_wallets), R.drawable.border_yellow)
    val container = getLayoutInflater.inflate(R.layout.frag_linear_layout, null).asInstanceOf[LinearLayout]
    val alert = mkCheckForm(alert => runAnd(alert.dismiss)(onOk), none, titleBodyAsViewBuilder(title.view, container), dialog_ok, dialog_cancel)
    var selected = Set.empty[ExtendedPublicKey]

    for {
      spec <- ElectrumWallet.specs.values if spec.spendable
      card = ChainCard(me, spec.data.keys.ewt.xPub)
    } {
      card.wrap setOnClickListener onButtonTap {
        selected = if (selected contains card.exPub) selected - card.exPub else selected + card.exPub
        val bgRes = if (selected contains card.exPub) R.drawable.border_card_signing_on else R.color.cardBitcoinSigning
        val totalCanSend = selected.flatMap(ElectrumWallet.specs.get).map(_.info.lastBalance).sum.toMilliSatoshi
        val human = WalletApp.denom.parsedWithSignTT(totalCanSend, mainColor = cardIn, zeroColor = cardZero)
        if (totalCanSend > 0L.msat) info.setText(s"∑ $human".html) else info.setText(select_wallets)
        updatePopupButton(getPositiveButton(alert), selected.nonEmpty)
        card.update(bgRes)
      }

      container.addView(card.view)
      card.update(R.color.cardBitcoinSigning)
      card.unPad
    }

    updatePopupButton(getPositiveButton(alert), isEnabled = false)
    def onOk: Unit
  }
}

trait BaseCheckActivity extends BaseActivity { me =>
  def PROCEED(state: Bundle): Unit

  override def START(state: Bundle): Unit = {
    if (WalletApp.isAlive && WalletApp.isOperational) PROCEED(state) else {
      // The way Android works is we can get some objects nullified when restoring from background
      // when that happens we make sure to free all remaining resources and start from scratch
      WalletApp.freePossiblyUsedRuntimeResouces
      exitTo(ClassNames.hubActivityClass)
    }
  }
}

trait QRActivity extends BaseCheckActivity { me =>
  def shareData(bitmap: Bitmap, bech32: String): Unit = {
    val paymentRequestFilePath = new File(getCacheDir, "images")
    if (!paymentRequestFilePath.isFile) paymentRequestFilePath.mkdirs
    val out = new FileOutputStream(s"$paymentRequestFilePath/qr.png")
    bitmap.compress(Bitmap.CompressFormat.PNG, 85, out)
    out.close

    val savedFile = new File(paymentRequestFilePath, "qr.png")
    val fileURI = FileProvider.getUriForFile(me, "com.btcontract.wallet", savedFile)
    val share = new Intent setAction Intent.ACTION_SEND setType "text/plain" addFlags Intent.FLAG_GRANT_READ_URI_PERMISSION
    share.putExtra(Intent.EXTRA_TEXT, bech32).putExtra(Intent.EXTRA_STREAM, fileURI).setDataAndType(fileURI, getContentResolver getType fileURI)
    me startActivity Intent.createChooser(share, "Choose an app")
  }

  class QRViewHolder(itemView: View) extends RecyclerView.ViewHolder(itemView) {
    val qrCode: ImageView = itemView.findViewById(R.id.qrCode).asInstanceOf[ImageView]
    val qrLabel: TextView = itemView.findViewById(R.id.qrLabel).asInstanceOf[TextView]
    val qrShare: AppCompatButton = itemView.findViewById(R.id.qrShare).asInstanceOf[AppCompatButton]
    val qrEdit: AppCompatButton = itemView.findViewById(R.id.qrEdit).asInstanceOf[AppCompatButton]
    val qrCopy: AppCompatButton = itemView.findViewById(R.id.qrCopy).asInstanceOf[AppCompatButton]
  }
}

object QRActivity {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(data: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size, hints)
    val (width, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](width * height)

    for {
      xPos <- 0 until width
      yPos <- 0 until height
      isBlack = bitMatrix.get(xPos, yPos)
      color = if (isBlack) Color.BLACK else Color.WHITE
    } pixels(yPos * width + xPos) = color

    val qrBitmap = Bitmap.createBitmap(width, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, size, 0, 0, width, height)
    qrBitmap
  }
}

// Cards

trait WalletCard {
  val host: BaseActivity
  def update(backgroundRes: Int): Unit

  def unPad: Unit = {
    val padding = paddingWrap.getPaddingTop
    paddingWrap.setPadding(padding, padding, padding, 0)
  }

  val view: LinearLayout = host.getLayoutInflater.inflate(R.layout.frag_card, null).asInstanceOf[LinearLayout]
  val paddingWrap: LinearLayout = view.findViewById(R.id.paddingWrap).asInstanceOf[LinearLayout]
  val doActionTip: ImageView = view.findViewById(R.id.doActionTip).asInstanceOf[ImageView]
  val wrap: CardView = view.findViewById(R.id.wrap).asInstanceOf[CardView]

  val cardContainer: View = view.findViewById(R.id.cardContainer).asInstanceOf[View]
  val cardTextLabel: TextView = view.findViewById(R.id.cardTextLabel).asInstanceOf[TextView]
  val cardTextNotice: TextView = view.findViewById(R.id.cardTextNotice).asInstanceOf[TextView]
  val cardImageLabel: ImageView = view.findViewById(R.id.cardImageLabel).asInstanceOf[ImageView]

  val balanceWrap: LinearLayout = view.findViewById(R.id.balanceWrap).asInstanceOf[LinearLayout]
  val balanceFiat: TextView = view.findViewById(R.id.balanceFiat).asInstanceOf[TextView]
  val balance: TextView = view.findViewById(R.id.balance).asInstanceOf[TextView]
}

case class ChainCard(host: BaseActivity, exPub: ExtendedPublicKey) extends WalletCard {
  override def update(backgroundRes: Int): Unit = ElectrumWallet.specs.get(exPub).foreach { spec =>
    if (spec.info.core.attachedMaster.isDefined) cardTextNotice setText attached_wallet else cardTextNotice setText tap_to_receive
    balance setText WalletApp.denom.parsedWithSignTT(spec.info.lastBalance.toMilliSatoshi, "#FFFFFF", signCardZero).html
    cardTextLabel setText spec.info.label.asSome.filter(_.trim.nonEmpty).getOrElse(spec.info.core.walletType)
    balanceFiat setText WalletApp.currentMsatInFiatHuman(spec.info.lastBalance.toMilliSatoshi)

    val warm = spec.info.lastBalance > Satoshi(0L)
    host.setVisMany(warm -> balanceWrap, !warm -> doActionTip)
    cardContainer setBackgroundResource backgroundRes
  }
}

case class TacAdvCard(host: BaseActivity) extends WalletCard {
  override def update(backgroundResource: Int): Unit = {
    host.setVisMany(false -> cardTextLabel, true -> cardImageLabel, true -> doActionTip)
    cardContainer setBackgroundResource backgroundResource
    cardImageLabel setImageResource R.drawable.talogo
    cardTextNotice setText tacadv_hint
  }
}