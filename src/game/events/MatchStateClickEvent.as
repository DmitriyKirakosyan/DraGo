/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 5/27/12
 * Time: 12:27 AM
 * To change this template use File | Settings | File Templates.
 */
package game.events {
import flash.events.Event;

public class MatchStateClickEvent extends Event {
	public var x:int;
	public var y:int;

	public static const CLICK:String = "click";
	public static const UNCLICK:String = "unclick";

	public function MatchStateClickEvent(type:String, x:int, y:int) {
		super(type);
		this.x = x;
		this.y = y;
	}
}
}
