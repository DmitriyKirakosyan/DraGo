/**
 * Created by : Dmitry
 * Date: 4/22/12
 * Time: 10:26 PM
 */
package controller {
import core.enum.WindowsENUM;
import core.window.WindowManager;

import flash.display.Sprite;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.MouseEvent;
import flash.geom.Point;

import game.board.view.BoardView;
import game.board.model.BoardModel;
import game.Player;
import game.estimate.MatchEstimator;
import game.events.BoardViewEvent;
import game.events.MatchStateClickEvent;
import game.events.MatchStateEvent;
import game.iface.window.EndGameWindow;
import game.iface.window.GameInterface;
import game.iface.window.panel.GameButtonEvent;
import game.staticModel.MatchState;
import game.staticModel.UserState;
import game.stone.StoneVO;
import game.events.PlayerMoveEvent;

import rpc.GameRpc;

import scene.IScene;
import scene.SceneEvent;

public class GameProcess extends EventDispatcher implements IScene {
	private var _container:Sprite;
	private var _gameContainer:Sprite;
	private var _boardView:BoardView;
	private var _boardModel:BoardModel;
	private var _gameInterface:GameInterface;

	private var _whitePlayer:Player;
	private var _blackPlayer:Player;

	private var _estimator:MatchEstimator;

	public static const ROWS_NUM:int = 11;

	public function GameProcess(container:Sprite):void {
		_container = container;
		init();
		_estimator =  new MatchEstimator(_boardModel);
		MatchState.instance.addEventListener(MatchStateEvent.PHASE_CHANGED, onMatchPhaseChanged);
		MatchState.instance.addEventListener(MatchStateEvent.BASIC_PHASE_CHANGED_ON_MAIN_PHASE, onStartMainPhase);
		MatchState.instance.addEventListener(MatchStateEvent.GAME_STOPPED, onGameStopped);
	}

	public function set whitePlayer(value:Player):void {
		if (_whitePlayer) { trace("WARN! player already exists ! [GameController.whitePlayer]"); }
		_whitePlayer = value;
		if (_whitePlayer.home) { _whitePlayer.setBoardView(_boardView);
		}
		addPlayerListeners(_whitePlayer);
	}
	public function set blackPlayer(value:Player):void {
		if (_blackPlayer) { trace("WARN! player already exists ! [GameController.blackPlayer]"); }
		_blackPlayer = value;
		if (_blackPlayer.home) { _blackPlayer.setBoardView(_boardView);
		}
		addPlayerListeners(_blackPlayer);
	}

	public function open():void {
		_container.addChild(_gameContainer);
		_gameContainer.addChild(_boardView);
		_gameInterface.open();
		whitePlayer =  new Player(MatchState.instance.whitePlayer);
		blackPlayer = new Player(MatchState.instance.blackPlayer);
		startGame();
	}
	public function close():void {
		_gameInterface.close();
		_container.removeChild(_gameContainer);
		_whitePlayer.remove();
		_blackPlayer.remove();
		_boardModel.removeAllStones();
		_boardView.clear();
	}

	/* Internal functions */

	private function startGame():void {
	}
	private function endGame():void {
		trace("end game");
		estimate();
		MatchState.instance.addEventListener(MatchStateClickEvent.CLICK, onNewClick);
		MatchState.instance.addEventListener(MatchStateClickEvent.UNCLICK, onNewUnClick);
	}

	private function onStartMainPhase(event:MatchStateEvent):void {
		_boardView.clear();
		_boardModel.removeAllStones();
		MatchState.instance.addEventListener(MatchStateEvent.UPDATED, onMatchUpdatedInMainPhase);
	}
	private function onMatchUpdatedInMainPhase(event:MatchStateEvent):void {
		MatchState.instance.removeEventListener(MatchStateEvent.UPDATED, onMatchUpdatedInMainPhase)
		for each (var stoneVO:StoneVO in MatchState.instance.stones) {
			makeMove(stoneVO);
		}
	}

	private function onMatchPhaseChanged(event:MatchStateEvent):void {
		if (MatchState.instance.phase == MatchState.MAIN_PHASE) {
		} else if (MatchState.instance.phase == MatchState.END_PHASE) {
			endGame();
		}
	}

	private function onGameStopped(event:MatchStateEvent):void {
		var endWindow:EndGameWindow = WindowManager.instance.getWindow(WindowsENUM.End_WINDOW) as EndGameWindow;
		endWindow.okBtn.addEventListener(MouseEvent.CLICK, onEndBtnClick);
		WindowManager.instance.showWindow(WindowsENUM.End_WINDOW);
	}
	private function onEndBtnClick(event:MouseEvent):void {
		var endWindow:EndGameWindow = WindowManager.instance.getWindow(WindowsENUM.End_WINDOW) as EndGameWindow;
		endWindow.okBtn.removeEventListener(MouseEvent.CLICK, onEndBtnClick);
		dispatchEvent(new SceneEvent(SceneEvent.SWITCH_ME, this));
	}

	private function onNewClick(event:MatchStateClickEvent):void {
		var click:Point = new Point(event.x, event.y);
		if (!_estimator.hasCapturedPoint(click)) {
			_estimator.addCapturedStone(click);
			estimate();
		}
	}
	private function onNewUnClick(event:MatchStateClickEvent):void {
		var click:Point = new Point(event.x, event.y);
		if (_estimator.hasCapturedPoint(click)) {
			_estimator.removeCapturedStone(click);
			estimate();
		}
	}

	private function addPlayerListeners(player:Player):void {
		player.addEventListener(PlayerMoveEvent.MOVE, onPlayerMove);
	}
	private function removePlayerListeners(player:Player):void {
		player.removeEventListener(PlayerMoveEvent.MOVE, onPlayerMove);
	}

	private function init():void {
		_boardModel = BoardModel.standartBoard();
		_gameContainer = new Sprite();
		_gameContainer.graphics.beginFill(0xCDC5BF);
		_gameContainer.graphics.drawRect(0, 0, Main.WIDTH, Main.HEIGHT);
		_gameContainer.graphics.endFill();
		_boardView = new BoardView(_boardModel);
		_boardView.addEventListener(BoardViewEvent.CLICK, onBoardViewClick);
		_boardView.y = 20;
		initInterface();
	}

	private function initInterface():void {
		_gameInterface = new GameInterface(_container);
		_gameInterface.addEventListener(GameButtonEvent.PASS, onPass);
		_gameInterface.addEventListener(GameButtonEvent.RESIGN, onResign);
		_gameInterface.addEventListener(GameButtonEvent.FINISH_GAME, onFinishGame);
		_gameInterface.addEventListener(GameButtonEvent.SHOW_HIDDEN, onShowHidden);
	}

	private function onBoardViewClick(event:BoardViewEvent):void {
		if (!_boardModel.isValidPoint(event.cellX, event.cellY)) { return; }
		if (_boardModel.emptyPoint(event.cellX, event.cellY) ||
				MatchState.instance.phase != MatchState.END_PHASE) {
			return;
		}

		var point:Point = new Point(event.cellX, event.cellY);
		if (_estimator.hasCapturedPoint(point)) {
			var capturedPoints:Vector.<StoneVO> = _estimator.getCapturedPoints(point);
			_estimator.removeCapturedStone(point);
			var points:Array = [];
			for each (var stone:StoneVO in capturedPoints) {
				points.push({x: stone.x, y: stone.y});
			}
			GameRpc.instance.unclick_capture_stone(points, null, null);
		} else {
			_estimator.addCapturedStone(point);
			GameRpc.instance.click_capture_stone(point.x, point.y, null, null);
		}
		estimate();
	}

	private function accessToMove(homePlayer:Player):Boolean {
		if (!_boardModel.lastStoneVO) {
			if (MatchState.instance.movePlayer != homePlayer.vo.userId) {
				return false;
			}
		} else if (MatchState.instance.phase == MatchState.MAIN_PHASE &&
								_boardModel.lastStoneVO.color == homePlayer.vo.color) {
			return false;
		}
		if (MatchState.instance.phase == MatchState.BASIC_PHASE && _boardModel.getNumStones() == MatchState.NUM_BASIC_STONES) {
			return false;
		}
		if (MatchState.instance.phase == MatchState.END_PHASE) { return false; }
		return true;
	}

	private function onPlayerMove(event:PlayerMoveEvent):void {
		var player:Player = event.target as Player;

		//check for access to move
		if (player.home && (!(_whitePlayer.home && _blackPlayer.home) && !accessToMove(player))) {
			return;
		}

		//check for valid point
		if (!_boardModel.isValidPoint(event.stone.x, event.stone.y) && !event.stone.pass) { return; }

		if (player.home) {
			event.stone.basic = MatchState.instance.phase == MatchState.BASIC_PHASE;
		}

		if (makeMove(event.stone)) {
			if (player.home) {
				GameRpc.instance.makeMove(event.stone.x, event.stone.y, event.stone.hidden, null, null);
			}
		}
	}

	private function estimate():void {
		_estimator.estimate();
		trace("length white points : " + _estimator.whitePoints.length);
		trace("length black points : " + _estimator.blackPoints.length);
		trace("white counts : " + _estimator.whiteCounts);
		trace("black counts : " + _estimator.blackCounts);
		trace("[GameController.estimate]");
		_boardView.cleanTerritory();
		_boardView.showTerritory(StoneVO.WHITE, _estimator.whitePoints);
		_boardView.showTerritory(StoneVO.BLACK, _estimator.blackPoints);
	}

	private function makeMove(stoneVO:StoneVO):Boolean {
		//check for pass
		if (stoneVO.pass) {
			_boardModel.addPass(stoneVO);
			_boardView.setPassMove();
			return true;
		}

		//add stone to board
		if (_boardModel.emptyPoint(stoneVO.x, stoneVO.y) && !_boardModel.isSelfKilled(stoneVO)) {
			_boardModel.addStone(stoneVO);
			placeStoneOnBoard(stoneVO);
			checkDeadStones();
		} else {
			if (_boardModel.getStone(stoneVO.x, stoneVO.y) && _boardModel.getStone(stoneVO.x, stoneVO.y).hidden) {
				_boardView.justShowHiddenStone(_boardModel.getStone(stoneVO.x, stoneVO.y));
			}
			return false;
		}

		//countable point
		(stoneVO.color == StoneVO.WHITE) ?
			_estimator.addWhiteCounts(_boardModel.getCountOfPoint(stoneVO.x, stoneVO.y)) :
			_estimator.addBlackCounts(_boardModel.getCountOfPoint(stoneVO.x, stoneVO.y));

		return true;
	}

	private function placeStoneOnBoard(stoneVO:StoneVO):void {
		if (!stoneVO.hidden || getPlayerByColor(stoneVO.color).vo.userId == UserState.instance.userId) {
			_boardView.addStone(stoneVO);
		} else {
			_boardView.showEnemyHiddenStoneEffect();
		}
	}

	private function checkDeadStones():void {
		var deadStones:Vector.<StoneVO> = _boardModel.getDeadStones();
		if (deadStones.length > 0) {

			(deadStones[0].color == StoneVO.WHITE) ?
				_estimator.addBlackCounts(deadStones.length) :
				_estimator.addWhiteCounts(deadStones.length);

			_boardModel.removeStones(deadStones);
			if (_boardModel.hiddenStones.length > 0) {
				_boardView.showHiddenStonesThenRemoveDeads(_boardModel.hiddenStones, deadStones);
				_boardModel.cleanHiddenStones();
			} else {
				_boardView.removeStones(deadStones);
			}
		}
	}

	private function onPass(event:Event):void {
		if (MatchState.instance.phase == MatchState.MAIN_PHASE) {
			_boardModel.addPass(new StoneVO(homePlayer.vo.color, -1, -1, false, false, -1, true));
			_boardView.setPassMove();
			GameRpc.instance.pass(null, null);
		}
	}
	private function onResign(event:Event):void {

	}
	private function onFinishGame(event:Event):void {
		if (MatchState.instance.phase == MatchState.END_PHASE) {
			var win:Boolean = (_whitePlayer.home) ?
				(_estimator.whiteCounts) >= (_estimator.blackCounts) :
				(_estimator.whiteCounts) < (_estimator.blackCounts);

			trace("i win : " + win + " [GameController.onClick]");
			GameRpc.instance.set_result_opinion(win, null, null);
			var endWindow:EndGameWindow = WindowManager.instance.getWindow(WindowsENUM.End_WINDOW) as EndGameWindow;
			endWindow.setResultText(win ? "THIS IS WIN!!" : "THIS IS LOSE!!");
		}
	}

	private function onShowHidden(event:Event):void {

	}

	private function getPlayerByColor(color:uint):Player {
		return _whitePlayer.vo.color == color ? _whitePlayer : _blackPlayer;
	}

	private function get homePlayer():Player {
		return _whitePlayer.home ? _whitePlayer : _blackPlayer;
	}

}
}
