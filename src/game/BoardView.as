/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 7:57 PM
 */
package game {
import controller.*;

import flash.display.Sprite;
import flash.events.MouseEvent;

import game.events.BoardViewEvent;
import game.stone.StoneVO;
import game.stone.StoneView;

public class BoardView extends Sprite {
	public static const CELL_WIDTH:Number = 40;
	public static const CELL_HEIGHT:Number = 40;
	public static const BORDER_WIDTH:Number = 20;

	private var _stones:Vector.<StoneView>;

	public function BoardView() {
		super();
		init();
	}

	public function clear():void {

	}

	public function addStone(stoneVO:StoneVO):void {
		var stoneView:StoneView = new StoneView(stoneVO);
		stoneView.x = stoneVO.x * CELL_WIDTH + BORDER_WIDTH;
		stoneView.y = stoneVO.y * CELL_HEIGHT + BORDER_WIDTH;
		if (!_stones) { _stones = new Vector.<StoneView>(); }
		_stones.push(stoneView);
		addChild(stoneView);
	}



	public function removeStones(stoneVOs:Vector.<StoneVO>):void {
		for each (var stoneVO:StoneVO in stoneVOs) {
			removeStone(stoneVO);
		}
	}

	private function removeStone(stoneVO:StoneVO):void {
		var stoneView:StoneView = getStoneByVO(stoneVO);
		if (stoneView) {
			var index:int = _stones.indexOf(stoneView);
			if (index != -1) {
				_stones.splice(index, 1);
			}
			removeChild(stoneView);
		}
	}

	private function getStoneByVO(stoneVO:StoneVO):StoneView {
		for each (var stoneView:StoneView in _stones) {
			if (stoneView.vo == stoneVO) {
				return stoneView;
			}
		}
		return null;
	}

	private function init():void {
		drawBackground();
		drawLines();
		drawBoarder();
		addListeners();
	}

	private function addListeners():void {
		super.addEventListener(MouseEvent.CLICK, onMouseClick);
	}

	private function onMouseClick(event:MouseEvent):void {
		var stoneX:int = (event.localX + CELL_WIDTH/2 - BORDER_WIDTH) / CELL_WIDTH;
		var stoneY:int = (event.localY + CELL_HEIGHT/2 - BORDER_WIDTH) / CELL_HEIGHT;
		if (stoneX >= 0 && stoneX <= GameController.ROWS_NUM &&
				stoneY >= 0 && stoneY <= GameController.ROWS_NUM) {
			dispatchEvent(new BoardViewEvent(BoardViewEvent.CLICK, stoneX, stoneY));
		}
	}

	private function drawBackground():void {
		this.graphics.beginFill(0x2fca43);
		this.graphics.drawRect(0, 0, boardWidth, boardHeight);
		this.graphics.endFill();
	}
	private function drawLines():void {
		this.graphics.lineStyle(1, 0);
		//horizontal
		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			this.graphics.moveTo(BORDER_WIDTH + i * CELL_WIDTH, BORDER_WIDTH);
			this.graphics.lineTo(BORDER_WIDTH + i * CELL_WIDTH, boardHeight - BORDER_WIDTH);
		}
		//vertical
		for (var j:int = 0; j < GameController.ROWS_NUM; ++j) {
			this.graphics.moveTo(BORDER_WIDTH, BORDER_WIDTH + j * CELL_HEIGHT);
			this.graphics.lineTo(boardWidth - BORDER_WIDTH, BORDER_WIDTH + j * CELL_HEIGHT);
		}
	}
	private function drawBoarder():void {
		this.graphics.lineStyle(2, 0);
		this.graphics.moveTo(0, 0);
		this.graphics.lineTo(0, boardHeight);
		this.graphics.lineTo(boardWidth, boardHeight);
		this.graphics.lineTo(boardWidth, 0);
		this.graphics.lineTo(0, 0);
	}

	public function get boardWidth():Number {
		return (GameController.ROWS_NUM-1) * CELL_WIDTH + BORDER_WIDTH*2;
	}
	public function get boardHeight():Number {
		return (GameController.ROWS_NUM-1) * CELL_HEIGHT + BORDER_WIDTH*2;
	}

}
}
