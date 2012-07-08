/**
 * User: dima
 * Date: 7/8/12
 * Time: 9:49 PM
 */
package game.board {
public class CountablePoint {
	private var _x:int;
	private var _y:int;
	private var _count:int;

	public function CountablePoint(x:int, y:int, count:int):void {
		_x = x;
		_y = y;
		_count = count;
	}

	public function get x():int { return _x; }
	public function get y():int { return _y; }
	public function get count():int { return _count; }

}
}
