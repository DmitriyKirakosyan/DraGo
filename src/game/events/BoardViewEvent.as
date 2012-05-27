/**
 * Created by : Dmitry
 * Date: 4/25/12
 * Time: 2:53 PM
 */
package game.events {
import flash.events.Event;

public class BoardViewEvent extends Event {
	public var cellX:int;
	public var cellY:int;
	public var shiftKey:Boolean;

	public static const CLICK:String = "boardClick";
	public function BoardViewEvent(type:String, x:int, y:int, shiftKey:Boolean = false):void {
		super(type);
		cellX = x;
		cellY = y;
		this.shiftKey = shiftKey;
	}
}
}
