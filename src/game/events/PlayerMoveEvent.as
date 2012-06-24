/**
 * Created by : Dmitry
 * Date: 4/26/12
 * Time: 4:43 PM
 */
package game.events {
import flash.events.Event;

import game.stone.StoneVO;

public class PlayerMoveEvent extends Event {
	public var stone:StoneVO;

	public static const MOVE:String = "move";
	public function PlayerMoveEvent(type:String, stoneVO:StoneVO):void {
		super(type);
		this.stone = stoneVO;
	}
}
}
