/**
 * Created by : Dmitry
 * Date: 4/26/12
 * Time: 4:43 PM
 */
package game.events {
import flash.events.Event;

public class PlayerEvent extends Event {
	public var x:int;
	public var y:int;

	public static const MOVE:String = "move";
	public function PlayerEvent(type:String, x:int, y:int) {
		super(type);
		this.x = x;
		this.y = y;
	}
}
}
