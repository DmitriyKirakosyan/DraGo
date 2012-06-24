/**
 * Created by : Dmitry
 * Date: 4/25/12
 * Time: 2:36 PM
 */
package game.stone {

public class StoneVO {
	private var _color:uint;
	private var _number:int;
	private var _x:int;
	private var _y:int;
	private var _hidden:Boolean;
	private var _basic:Boolean;
	private var _pass:Boolean;

	public static const WHITE:uint = 0;
	public static const BLACK:uint = 1;

	public static function createStoneByObject(object:Object):StoneVO {
		var color:uint = object["color"] == "white" ? WHITE : BLACK;
		return new StoneVO(color, object["x"], object["y"], object["hidden"], object["basic"], object["number"], object["pass"]);
	}

	public function StoneVO(color:uint, x:int, y:int, hidden:Boolean = false, basic:Boolean = false, number:int = -1, pass:Boolean = false) {
		_color = color;
		_x = x;
		_y = y;
		_hidden = hidden;
		_basic = basic;
		_number = number;
		_pass = pass;
	}

	public function get color():uint { return _color; }
	public function get basic():Boolean { return _basic; }
	public function get hidden():Boolean { return _hidden }
	public function get pass():Boolean { return _pass; }

	public function get x():int { return _x; }
	public function get y():int { return _y; }

	public function get number():int { return _number; }

	public function set basic(value:Boolean):void {
		_basic = value;
	}

}
}
