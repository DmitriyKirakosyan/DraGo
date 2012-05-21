/**
 * Created by : Dmitry
 * Date: 4/25/12
 * Time: 2:36 PM
 */
package game.stone {
public class StoneVO {
	private var _color:uint;
	private var _x:int;
	private var _y:int;
	private var _hidden:Boolean;
	private var _basic:Boolean;

	public static const WHITE:uint = 0;
	public static const BLACK:uint = 1;

	public static function createStoneByObject(object:Object):StoneVO {
		return new StoneVO(object["color"], object["x"], object["y"], object["hidden"], object["basic"]);
	}

	public static function createWhiteStone(x:int, y:int):StoneVO {
		return new StoneVO(WHITE, x, y);
	}
	public static function createBlackStone(x:int, y:int):StoneVO {
		return new StoneVO(BLACK, x, y);
	}

	public function StoneVO(color:uint, x:int, y:int, hidden:Boolean = false, basic:Boolean = false) {
		_color = color;
		_x = x;
		_y = y;
		_hidden = hidden;
		_basic = basic;
	}

	public function get color():uint { return _color; }
	public function isWhite():Boolean { return _color == WHITE; }
	public function isBlack():Boolean { return _color == BLACK; }

	public function get x():int { return _x; }
	public function get y():int { return _y; }
}
}
