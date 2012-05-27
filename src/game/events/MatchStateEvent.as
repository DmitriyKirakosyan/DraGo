/**
 * Created by : Dmitry
 * Date: 5/18/12
 * Time: 1:28 AM
 */
package game.events {
import flash.events.Event;

public class MatchStateEvent extends Event{
	public static const NEW_STONE:String = "newStone";
	public static const CHANGE_MOVE_PLAYER:String = "changeMovePlayer";
	public static const GAME_STARTED:String = "gameStarted";
	public static const GAME_STOPPED:String = "gameStopped";
	public static const PHASE_CHANGED:String = "phaseChanged";
	public static const BASIC_PHASE_CHANGED_ON_MAIN_PHASE:String = "fromBasicToMain";
	public static const UPDATED:String = "updated";

	public function MatchStateEvent(type:String):void {
		super(type);
	}
}
}
