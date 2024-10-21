package com.btcontract.wallet.utils

import org.bouncycastle.util.encoders.Base64
import scodec.bits.ByteVector

object BIP322Data {
  val signRegex = "(?im).*?(bip322sign)([A-Za-z0-9+/=a-fA-F|-]+)".r.unanchored
  val verifyRegex = "(?im).*?(bip322verify)([A-Za-z0-9+/=a-fA-F|-]+)".r.unanchored
  def fromBase64String(raw64: String): String = new String(Base64 decode raw64)
  def toBase64String(raw: String): String = Base64.toBase64String(raw.getBytes)

  def parseVerify(raw: String): BIP322VerifyData = raw split '|' match {
    case Array(address, hash, sig64, "-") => BIP322VerifyData(message = None, address, ByteVector.fromValidHex(hash), sig64)
    case Array(address, hash, sig64, msg) => BIP322VerifyData(Some apply fromBase64String(msg), address, ByteVector.fromValidHex(hash), sig64)
    case _ => throw new RuntimeException
  }

  def parseSign(raw: String): BIP32SignData = raw split '|' match {
    case Array(message, address) => BIP32SignData(message, address)
    case _ => throw new RuntimeException
  }
}

case class BIP322VerifyData(message: Option[String], address: String, messageHash: ByteVector, signature64: String) {
  def serialize: String = s"bip322verify$address|${messageHash.toHex}|$signature64|${message.map(_.getBytes).map(Base64.toBase64String) getOrElse "-"}"
}

case class BIP32SignData(message64: String, address: String) {
  lazy val message: String = BIP322Data.fromBase64String(message64)
  def serialize: String = s"bip322sign$message64|$address"
}