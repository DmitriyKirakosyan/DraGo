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

	public static const CLICK:String = "boardClick";
	public function BoardViewEvent(type:String, x:int, y:int):void {
		super(type);
		cellX = x;
		cellY = y;
	}
}
}
