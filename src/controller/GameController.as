/**
 * Created by : Dmitry
 * Date: 4/22/12
 * Time: 10:26 PM
 */
package controller {
import flash.display.Sprite;
import flash.events.EventDispatcher;
import flash.events.MouseEvent;

import game.BoardView;
import game.GameModel;
import game.Player;
import game.estimate.MatchEstimator;
import game.events.MatchStateEvent;
import game.staticModel.MatchState;
import game.stone.StoneVO;
import game.events.PlayerEvent;

import rpc.GameRpc;

import scene.IScene;
import scene.SceneEvent;

public class GameController extends EventDispatcher implements IScene {
	private var _container:Sprite;
	private var _gameContainer:Sprite;
	private var _boardView:BoardView;
	private var _gameModel:GameModel;

	private var _whitePlayer:Player;
	private var _blackPlayer:Player;

	private var _playerToMove:Player;

	public static const ROWS_NUM:int = 11;

	public function GameController(container:Sprite):void {
		_container = container;
		initObjects();
		_gameModel = new GameModel();
		MatchState.instance.addEventListener(MatchStateEvent.CHANGE_MOVE_PLAYER, onMovePlayerChange);
		MatchState.instance.addEventListener(MatchStateEvent.PHASE_CHANGED, onMatchPhaseChanged);
		MatchState.instance.addEventListener(MatchStateEvent.GAME_STOPPED, onGameStopped);
	}

	public function set whitePlayer(value:Player):void {
		if (_whitePlayer) { trace("WARN! player already exists ! [GameController.whitePlayer]"); }
		_whitePlayer = value;
		if (_whitePlayer.home) { _whitePlayer.setBoardView(_boardView);
		} else {
			addPlayerListeners(_whitePlayer);
		}
	}
	public function set blackPlayer(value:Player):void {
		if (_blackPlayer) { trace("WARN! player already exists ! [GameController.blackPlayer]"); }
		_blackPlayer = value;
		if (_blackPlayer.home) { _blackPlayer.setBoardView(_boardView);
		} else {
			addPlayerListeners(_blackPlayer);
		}
	}

	public function open():void {
		_container.addChild(_gameContainer);
		_gameContainer.addChild(_boardView);
		addListeners();
		whitePlayer =  new Player(MatchState.instance.whitePlayer);
		blackPlayer = new Player(MatchState.instance.blackPlayer);
		startGame();
	}
	public function close():void {
	 	removeListeners();
		_container.removeChild(_gameContainer);
		_whitePlayer.remove();
		_blackPlayer.remove();
	}

	/* Internal functions */

	private function startGame():void {
	}

	private function onMovePlayerChange(event:MatchStateEvent):void {
		playerToMove(MatchState.instance.movePlayer == MatchState.instance.whitePlayer.userId ? _whitePlayer : _blackPlayer);
	}

	private function onMatchPhaseChanged(event:MatchStateEvent):void {
		if (MatchState.instance.phase == MatchState.MAIN_PHASE) {
			for each (var stoneVO:StoneVO in MatchState.instance.stones) {
				makeMove(stoneVO);
			}
			playerToMove(MatchState.instance.movePlayer == MatchState.instance.whitePlayer.userId ? _whitePlayer : _blackPlayer);
		} else if (MatchState.instance.phase == MatchState.END_PHASE) {
			playerToMove(null);
			trace("end game");
			estimate();
		}
	}

	private function onGameStopped(event:MatchStateEvent):void {
		trace("game stopped [GameController.onGameStopped]");
	}

	private function playerToMove(playerToMove:Player):void {
		trace("change player to move : " + playerToMove + " [GameController.playerToMove]");
		if (_playerToMove && _playerToMove.home) {
			removePlayerListeners(_playerToMove);
		}
		_playerToMove = playerToMove;
		if (_playerToMove && _playerToMove.home) {
			addPlayerListeners(playerToMove);
		}
	}

	private function addPlayerListeners(player:Player):void {
		player.addEventListener(PlayerEvent.MOVE, onPlayerMove);
	}
	private function removePlayerListeners(player:Player):void {
		player.removeEventListener(PlayerEvent.MOVE, onPlayerMove);
	}

	private function initObjects():void {
		_gameContainer = new Sprite();
		_gameContainer.graphics.beginFill(0xffffff);
		_gameContainer.graphics.drawRect(0, 0, Main.WIDTH, Main.HEIGHT);
		_gameContainer.graphics.endFill();
		_boardView = new BoardView();
		_boardView.addEventListener(MouseEvent.CLICK, onBoardViewClick);
		_boardView.x = 20;
		_boardView.y = 20;
	}

	private function onBoardViewClick(event:MouseEvent):void {
	}

	private function onPlayerMove(event:PlayerEvent):void {
		var color:uint = (event.target as Player).vo.color;
		var basic:Boolean = MatchState.instance.phase == MatchState.BASIC_PHASE;
		var stoneVO:StoneVO = new StoneVO(color, event.x, event.y, false, basic);
		makeMove(stoneVO);
		if ((event.target as Player).home) {
			GameRpc.instance.makeMove(stoneVO.x, stoneVO.y, false, null, null);
		}
		if (MatchState.instance.phase == MatchState.BASIC_PHASE) {
			if (_gameModel.getNumStones() == MatchState.NUM_BASIC_STONES) {
					playerToMove(null);
			}
		} else if ((event.target as Player).home) {
			playerToMove(null);
		}
	}

	private function estimate():void {
		var estimator:MatchEstimator =  new MatchEstimator(_gameModel);
		estimator.estimate();
		trace("length white points : " + estimator.whitePoints().length);
		trace("length black points : " + estimator.blackPoints().length);
		trace("[GameController.estimate]");
	}

	private function makeMove(stoneVO:StoneVO):void {
		if (_gameModel.canMove(stoneVO.x, stoneVO.y)) {
			_gameModel.addStone(stoneVO);
			_boardView.addStone(stoneVO);
			var deadStones:Vector.<StoneVO> = _gameModel.getDeadStones();
			if (deadStones.length > 0) {
				_gameModel.removeStones(deadStones);
				_boardView.removeStones(deadStones);
			}
			//switchPlayerMove();
		}
	}

	private function addListeners():void {
		_gameContainer.addEventListener(MouseEvent.CLICK, onClick);
	}
	private function removeListeners():void {
		_gameContainer.removeEventListener(MouseEvent.CLICK, onClick);
	}

	private function onClick(event:MouseEvent):void {
		if (event.ctrlKey) {
			trace("try pass");
			GameRpc.instance.pass(null, null);
		//	dispatchEvent(new SceneEvent(SceneEvent.SWITCH_ME, this));
		}
	}
}
}
