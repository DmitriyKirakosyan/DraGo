/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 6/13/12
 * Time: 2:35 AM
 * To change this template use File | Settings | File Templates.
 */
package game.iface.window {
import flash.display.Sprite;

import game.events.MatchStateEvent;
import game.iface.window.panel.GameButtonEvent;
import game.iface.window.panel.GameButtonsPanel;

import game.iface.window.panel.GamePlayersPanel;
import game.staticModel.MatchState;
import game.stone.StoneVO;

public class GameInterface extends Sprite {
	private var _container:Sprite;
	private var _playersPanel:GamePlayersPanel;
	private var _gameBtnsPanel:GameButtonsPanel;

	public function GameInterface(container:Sprite) {
		super();
		_container = container;
		init();
		MatchState.instance.addEventListener(MatchStateEvent.PHASE_CHANGED, onPhaseChanged);
	}

	public function open():void {
		_container.addChild(_playersPanel);
		_container.addChild(_gameBtnsPanel);
	}

	public function close():void {
		if (_container.contains(_playersPanel)) {
			_container.removeChild(_playersPanel);
		}
		_container.removeChild(_gameBtnsPanel);
	}

	private function init():void {
		_playersPanel = new GamePlayersPanel();
		_playersPanel.x = 31;
		_playersPanel.y = 3;

		_gameBtnsPanel = new GameButtonsPanel();
		_gameBtnsPanel.x = 470;
		_gameBtnsPanel.y = 40;
		_gameBtnsPanel.addEventListener(GameButtonEvent.FINISH_GAME, dispatchEvent);
		_gameBtnsPanel.addEventListener(GameButtonEvent.PASS, dispatchEvent);
		_gameBtnsPanel.addEventListener(GameButtonEvent.RESIGN, dispatchEvent);
		_gameBtnsPanel.addEventListener(GameButtonEvent.SHOW_HIDDEN, dispatchEvent);
	}

	private function onPhaseChanged(event:MatchStateEvent):void {
		MatchState.instance.removeEventListener(MatchStateEvent.CHANGE_MOVE_PLAYER, onChangeMovePlayer);
		_playersPanel.setNothingMove();
		if (MatchState.instance.phase == MatchState.MAIN_PHASE) {
			_playersPanel.changeMove(MatchState.instance.getMovePlayerColor());
			MatchState.instance.addEventListener(MatchStateEvent.CHANGE_MOVE_PLAYER, onChangeMovePlayer);
		}
	}

	private function onChangeMovePlayer(event:MatchStateEvent):void {
		_playersPanel.changeMove(MatchState.instance.getMovePlayerColor());
	}

}
}
