package firesword.wwd

import firesword.base.TextDecoder.decoder
import firesword.wwd.DataStream.ByteString
import firesword.wwd.Geometry.Rectangle

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, DataView, Uint8Array}

object OutputDataStream {

  class OutputStream() {
    private val _arrayBuffer = new ArrayBuffer(2 << 24) // 16 MiB

    private val _dataView = new DataView(_arrayBuffer);

    private var _offset = 0

    private val _littleEndian = true;

    def writeUint8(value: Short): Unit = {
      this._dataView.setUint8(this._offset, value);
      this._offset += 1
    }

    def writeInt32(value: Int): Unit = {
      this._dataView.setInt32(this._offset, value, this._littleEndian);
      this._offset += 4
    }

    def writeUint32(value: Long): Unit = {
      this._dataView.setUint32(this._offset, value, this._littleEndian);
      this._offset += 4;
    }

    def writeByteString(byteString: ByteString, length: Int): Unit = {
      val byteArray = byteString.byteArray
      val subArray = byteArray.subarray(0, length)

      subArray.foreach(byte => {
        _dataView.setUint8(_offset, byte)
        _offset += 1
      })

      _offset += (length - subArray.length)
    }

    //
    //      byteString.foreach(byte => {
    //
    //      })

    //      val fullByteString = new Uint8Array(this._arrayBuffer, this._offset, length);
    //      val firstZeroIndex = fullByteString.indexOf(0)
    //      val byteString: Uint8Array = if (firstZeroIndex >= 0)
    //        fullByteString.slice(0, firstZeroIndex) else
    //        fullByteString;
    //      this._offset += length;
    //      return new ByteString(byteString);
    //    }

    def writeByteStringNullTerminated(byteString: ByteString): Unit = {
      val byteArray = byteString.byteArray

      if (byteArray.toSeq.contains(0)) {
        throw new IllegalArgumentException("byteString contains NUL character(s)");
      }

      byteArray.toSeq.foreach(byte => {
        _dataView.setUint8(_offset, byte)
      })

      _offset += byteString.length
    }

    //
    //    def writeRectangle(): Rectangle = {
    //      Rectangle.fromBounds(
    //        this.writeInt32(),
    //        this.writeInt32(),
    //        this.writeInt32(),
    //        this.writeInt32(),
    //      );
    //    }
    //  }

    def toArrayBuffer(): ArrayBuffer =
      _arrayBuffer.slice(0, _offset)
  }
}
