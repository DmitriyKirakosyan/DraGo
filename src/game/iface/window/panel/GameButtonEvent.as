/**
 * Created by : Dmitry
 * Date: 6/17/12
 * Time: 1:29 AM
 */
package game.iface.window.panel {
import flash.events.Event;

public class GameButtonEvent extends Event {
	public static const PASS:String = "pass";
	public static const RESIGN:String = "resing";
	public static const SHOW_HIDDEN:String = "showHidden";
	public static const FINISH_GAME:String = "finishGame";

	public function GameButtonEvent(type:String):void {
		super(type);
	}
}
}
