/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 6/13/12
 * Time: 2:31 AM
 * To change this template use File | Settings | File Templates.
 */
package game.iface.window.panel {
import flash.display.Sprite;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;

import game.stone.StoneVO;

public class GamePlayersPanel extends Sprite {
	private const WIDTH:int = 400;
	private const HEIGHT:int = 15;

	private var _currentMove:uint;
	private var _moveSelector:Sprite;
	private var _moveTF:TextField;

	public function GamePlayersPanel():void {
		super();
		init();
	}

	private function init():void {
		drawPanel();
		createSelector();
		createMoveTF();
	}

	public function changeMove(color:uint):void {
		_currentMove = color;
		if (color == StoneVO.WHITE) {
			_moveSelector.x =  0;
			_moveTF.text = "white to move";
			_moveTF.textColor = 0;
			_moveTF.x = 10;
		} else {
			_moveSelector.x = WIDTH/2;
			_moveTF.text = "black to move";
			_moveTF.textColor = 0xffffff;
			_moveTF.x = WIDTH/2 + 10;
		}

		if (!contains(_moveSelector)) {
			addChild(_moveSelector);
			addChild(_moveTF);
		}
	}

	public function setNothingMove():void {
		if (contains(_moveSelector)) {
			removeChild(_moveSelector);
			removeChild(_moveTF);
		}
	}

	private function drawPanel():void {
		graphics.beginFill(0xffffff, .6);
		graphics.drawRect(0, 0, WIDTH/2, HEIGHT);
		graphics.beginFill(0, .6);
		graphics.drawRect(WIDTH/2, 0, WIDTH/2, HEIGHT);
		graphics.endFill();
		graphics.lineStyle(1, 0);
		graphics.drawRect(0, 0, WIDTH, HEIGHT);
	}

	private function createSelector():void {
		_moveSelector = new Sprite();
		_moveSelector.graphics.lineStyle(3, 0);
		_moveSelector.graphics.drawRect(0, 0, WIDTH/2, HEIGHT);
	}

	private function createMoveTF():void {
		_moveTF = new TextField();
		_moveTF.selectable = false;
		_moveTF.mouseEnabled = false;
		_moveTF.autoSize = TextFieldAutoSize.LEFT;
		_moveTF.y = 2;
	}

}
}
