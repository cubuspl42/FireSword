package firesword.wwd

import firesword.base.TextDecoder.decoder
import firesword.wwd.Geometry.Rectangle

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, DataView, Uint8Array}

object DataStream {
  @js.native
  trait Uint8ArrayExt extends Uint8Array {
    def indexOf(searchElement: Short): Int

    def slice(start: Int, end: Int): Uint8Array
  }

  implicit def implicitUint8ArrayExt(self: Uint8Array): Uint8ArrayExt =
    self.asInstanceOf[Uint8ArrayExt]


  class ByteString(val byteArray: Uint8Array) {
    override def toString: String = decoder.decode(byteArray)

    def decode(): String =
      decoder.decode(byteArray)
  }

  object ByteString {
    def empty(): ByteString =
      new ByteString(new Uint8Array(0))

    def decode(b: ByteString): String =
      b.decode()
  }

  class DataStream(_arrayBuffer: ArrayBuffer, initialOffset: Int = 0) {
    private val _dataView = new DataView(_arrayBuffer);

    private var _offset = initialOffset

    private val _littleEndian = true;


    def readInt32(): Int = {
      val value = this._dataView.getInt32(this._offset, this._littleEndian);
      this._offset += 4;
      return value;
    }

    def readUint32(): Long = {
      val value = this._dataView.getUint32(this._offset, this._littleEndian).toLong;
      this._offset += 4;
      return value;
    }

    def readByteString(length: Int): ByteString = {
      val fullByteString = new Uint8Array(this._arrayBuffer, this._offset, length);
      val firstZeroIndex = fullByteString.indexOf(0)
      val byteString: Uint8Array = if (firstZeroIndex >= 0)
        fullByteString.slice(0, firstZeroIndex) else
        fullByteString;
      this._offset += length;
      return new ByteString(byteString);
    }

    def readByteStringNullTerminated(): ByteString = {
      val bytes = js.Array[Short]();

      def readByte(): Unit = {
        val byte = this._dataView.getUint8(this._offset);
        this._offset += 1;
        if (byte != 0) {
          bytes.push(byte)
          readByte()
        }
      }

      readByte()

      new ByteString(new Uint8Array(bytes))
    }

    def readRectangle(): Rectangle = {
      Rectangle.fromBounds(
        this.readInt32(),
        this.readInt32(),
        this.readInt32(),
        this.readInt32(),
      );
    }
  }

}
