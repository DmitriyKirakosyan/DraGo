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
import game.staticModel.MatchInfo;
import game.stone.StoneVO;
import game.events.PlayerEvent;

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
	}

	public function set whitePlayer(value:Player):void {
		if (_whitePlayer) { trace("WARN! player already exists ! [GameController.whitePlayer]"); }
		_whitePlayer = value;
		if (_whitePlayer.home) { _whitePlayer.setBoardView(_boardView); }
	}
	public function set blackPlayer(value:Player):void {
		if (_blackPlayer) { trace("WARN! player already exists ! [GameController.blackPlayer]"); }
		_blackPlayer = value;
		if (_blackPlayer.home) { _blackPlayer.setBoardView(_boardView); }
	}

	public function open():void {
		_container.addChild(_gameContainer);
		_gameContainer.addChild(_boardView);
		addListeners();
		whitePlayer = new Player(MatchInfo.instance.whitePlayer.home);
		blackPlayer = new Player(MatchInfo.instance.blackPlayer.home);
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
		playerToMove(_blackPlayer);
	}

	private function playerToMove(playerToMove:Player):void {
		_playerToMove = playerToMove;
		if (playerToMove == _whitePlayer) {
			removePlayerListeners(_blackPlayer);
		} else {
			removePlayerListeners(_whitePlayer);
		}
		addPlayerListeners(playerToMove);
	}

	private function switchPlayerMove():void {
		if (_playerToMove == _whitePlayer) {
			playerToMove(_blackPlayer);
		} else {
			playerToMove(_whitePlayer);
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
		_gameContainer.graphics.beginFill(0xf33daa);
		_gameContainer.graphics.drawRect(0, 0, Main.WIDTH, Main.HEIGHT);
		_gameContainer.graphics.endFill();
		_boardView = new BoardView();
		_boardView.x = 20;
		_boardView.y = 20;
	}

	private function onPlayerMove(event:PlayerEvent):void {
		if (_gameModel.canMove(event.x, event.y)) {
			var color:uint = _playerToMove == _whitePlayer ? StoneVO.WHITE : StoneVO.BLACK;
			var stoneVO:StoneVO = new StoneVO(color, event.x, event.y);
			_gameModel.addStone(stoneVO);
			_boardView.addStone(stoneVO);
			var deadStones:Vector.<StoneVO> = _gameModel.getDeadStones();
			if (deadStones.length > 0) {
				_gameModel.removeStones(deadStones);
				_boardView.removeStones(deadStones);
			}
			switchPlayerMove();
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
			dispatchEvent(new SceneEvent(SceneEvent.SWITCH_ME, this));
		}
	}
}
}
