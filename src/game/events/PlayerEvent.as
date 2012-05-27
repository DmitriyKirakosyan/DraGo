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
	public var hidden:Boolean;

	public static const MOVE:String = "move";
	public function PlayerEvent(type:String, x:int, y:int, hidden:Boolean = false) {
		super(type);
		this.x = x;
		this.y = y;
		this.hidden = hidden;
	}
}
}
