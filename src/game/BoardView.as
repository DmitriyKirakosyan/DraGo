/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 7:57 PM
 */
package game {
import controller.*;

import flash.display.Sprite;

public class BoardView extends Sprite {
	public static const CELL_WIDTH:Number = 30;
	public static const CELL_HEIGHT:Number = 30;
	public static const BORDER_WIDTH:Number = 20;

	public function BoardView() {
		super();
		init();
	}


	private function init():void {
		drawBackground();
		drawLines();
		drawBoarder();
	}

	private function drawBackground():void {
		this.graphics.beginFill(0x2fca43);
		this.graphics.drawRect(0, 0, boardWidth, boardHeight);
		this.graphics.endFill();
	}
	private function drawLines():void {
		this.graphics.lineStyle(1, 0);
		//horizontal
		for (var i:int = 0; i < GameController.ROWS_NUM+1; ++i) {
			this.graphics.moveTo(BORDER_WIDTH + i * CELL_WIDTH, BORDER_WIDTH);
			this.graphics.lineTo(BORDER_WIDTH + i * CELL_WIDTH, boardHeight - BORDER_WIDTH);
		}
		//vertical
		for (var j:int = 0; j < GameController.ROWS_NUM+1; ++j) {
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
		return GameController.ROWS_NUM * CELL_WIDTH + BORDER_WIDTH*2;
	}
	public function get boardHeight():Number {
		return GameController.ROWS_NUM * CELL_HEIGHT + BORDER_WIDTH*2;
	}

}
}
