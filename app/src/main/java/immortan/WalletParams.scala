package immortan

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import com.softwaremill.quicklens._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.blockchain.electrum._
import fr.acinq.eclair.blockchain.electrum.db._
import immortan.crypto.CanBeShutDown
import immortan.sqlite._
import immortan.utils._
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.util.Try


object WalletParams {
  var secret: WalletSecret = _
  var chainHash: ByteVector32 = _
  var chainWallets: WalletExt = _
  var connectionProvider: ConnectionProvider = _
  var fiatRates: FiatRates = _
  var feeRates: FeeRates = _
  var logBag: SQLiteLog = _

  val blockCount: AtomicLong = new AtomicLong(0L)

  def isOperational: Boolean =
    null != chainHash && null != secret && null != chainWallets &&
      connectionProvider != null && null != fiatRates &&
      null != feeRates && null != logBag

  implicit val timeout: Timeout = Timeout(30.seconds)
  implicit val system: ActorSystem = ActorSystem("immortan-actor-system")
  implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global

  def loggedActor(childProps: Props, childName: String): ActorRef = system actorOf Props(classOf[LoggingSupervisor], childProps, childName)

  def addressToPubKeyScript(address: String): ByteVector = Script write addressToPublicKeyScript(address, chainHash)
}

case class WalletExt(wallets: List[ElectrumEclairWallet], catcher: ActorRef, sync: ActorRef, pool: ActorRef, params: WalletParameters) extends CanBeShutDown { me =>

  lazy val defaultWallet: ElectrumEclairWallet = wallets.find(_.isBuiltIn).get

  lazy val usableWallets: List[ElectrumEclairWallet] = wallets.filter(wallet => wallet.isSigning || wallet.hasFingerprint)

  def findByPubKey(pub: PublicKey): Option[ElectrumEclairWallet] = wallets.find(_.ewt.xPub.publicKey == pub)

  def makeSigningWalletParts(core: SigningWallet, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt = ElectrumWalletType.makeSigningType(tag = core.walletType, master = WalletParams.secret.keys.master, chainHash = WalletParams.chainHash)
    val walletRef = WalletParams.loggedActor(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-signing-wallet")
    val infoNoPersistent = CompleteChainWalletInfo(core, data = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    ElectrumEclairWallet(walletRef, ewt, infoNoPersistent)
  }

  def makeWatchingWallet84Parts(core: WatchingWallet, lastBalance: Satoshi, label: String): ElectrumEclairWallet = {
    val ewt: ElectrumWallet84 = new ElectrumWallet84(secrets = None, xPub = core.xPub, chainHash = WalletParams.chainHash)
    val walletRef = WalletParams.loggedActor(Props(classOf[ElectrumWallet], pool, sync, params, ewt), core.walletType + "-watching-wallet")
    val infoNoPersistent = CompleteChainWalletInfo(core, data = ByteVector.empty, lastBalance, label, isCoinControlOn = false)
    ElectrumEclairWallet(walletRef, ewt, infoNoPersistent)
  }

  def withFreshWallet(eclairWallet: ElectrumEclairWallet): WalletExt = {
    params.walletDb.addChainWallet(eclairWallet.info, params.emptyPersistentDataBytes, eclairWallet.ewt.xPub.publicKey)
    eclairWallet.walletRef ! params.emptyPersistentDataBytes
    sync ! ElectrumWallet.ChainFor(eclairWallet.walletRef)
    copy(wallets = eclairWallet :: wallets)
  }

  def withoutWallet(wallet: ElectrumEclairWallet): WalletExt = {
    require(wallet.info.core.isRemovable, "Wallet is not removable")
    params.walletDb.remove(pub = wallet.ewt.xPub.publicKey)
    params.txDb.removeByPub(xPub = wallet.ewt.xPub)
    val wallets1 = wallets diff List(wallet)
    wallet.walletRef ! PoisonPill
    copy(wallets = wallets1)
  }

  def withNewLabel(label: String)(wallet1: ElectrumEclairWallet): WalletExt = {
    require(!wallet1.isBuiltIn, "Can not re-label a default built in chain wallet")
    def sameXPub(wallet: ElectrumEclairWallet): Boolean = wallet.ewt.xPub == wallet1.ewt.xPub
    params.walletDb.updateLabel(label, pub = wallet1.ewt.xPub.publicKey)
    me.modify(_.wallets.eachWhere(sameXPub).info.label).setTo(label)
  }

  override def becomeShutDown: Unit = {
    val actors = List(catcher, sync, pool)
    val allActors = wallets.map(_.walletRef) ++ actors
    allActors.foreach(_ ! PoisonPill)
  }
}

case class WalletSecret(keys: LightningNodeKeys, mnemonic: List[String], seed: ByteVector)

// Interfaces

trait DataBag {
  def putSecret(secret: WalletSecret)
  def tryGetSecret: Try[WalletSecret]

  def putFiatRatesInfo(data: FiatRatesInfo)
  def tryGetFiatRatesInfo: Try[FiatRatesInfo]

  def putFeeRatesInfo(data: FeeRatesInfo)
  def tryGetFeeRatesInfo: Try[FeeRatesInfo]
}