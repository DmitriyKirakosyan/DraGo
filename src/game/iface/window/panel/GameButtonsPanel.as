/**
 * Created by : Dmitry
 * Date: 6/17/12
 * Time: 1:19 AM
 */
package game.iface.window.panel {
import com.bit101.components.PushButton;

import flash.display.Sprite;
import flash.events.MouseEvent;

public class GameButtonsPanel extends Sprite {
	private var _passBtn:PushButton;
	private var _resignBtn:PushButton;
	private var _finishGameBtn:PushButton;
	private var _showHiddenBtn:PushButton;

	public function GameButtonsPanel() {
		super();
		init();
	}

	private function init():void {
		_passBtn = new PushButton(this, 0, 0, "Pass", onPass);
		_resignBtn = new PushButton(this, 0, 30, "Resign", onResign);
		_finishGameBtn = new PushButton(this, 0, 60, "Finish", onFinish);
		_showHiddenBtn = new PushButton(this, 0, 90, "Show", onShowHidden);
	}

	private function onPass(event:MouseEvent):void {
		dispatchEvent(new GameButtonEvent(GameButtonEvent.PASS));
	}
	private function onResign(event:MouseEvent):void {
		dispatchEvent(new GameButtonEvent(GameButtonEvent.RESIGN));
	}
	private function onFinish(event:MouseEvent):void {
		dispatchEvent(new GameButtonEvent(GameButtonEvent.FINISH_GAME));
	}
	private function onShowHidden(event:MouseEvent):void {
		dispatchEvent(new GameButtonEvent(GameButtonEvent.SHOW_HIDDEN));
	}

}
}
