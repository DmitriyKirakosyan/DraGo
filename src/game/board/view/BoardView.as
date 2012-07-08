/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 7:57 PM
 */
package game.board.view {
import com.greensock.TimelineMax;
import com.greensock.TweenMax;

import controller.*;

import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.geom.Point;

import game.board.model.BoardModel;
import game.board.model.CountablePoint;

import game.events.BoardViewEvent;
import game.stone.StoneVO;
import game.stone.StoneView;

import org.osmf.layout.PaddingLayoutMetadata;
import org.osmf.metadata.TimelineMarker;

public class BoardView extends Sprite {
	public static const CELL_WIDTH:Number = 36;
	public static const CELL_HEIGHT:Number = 36;
	public static const BORDER_WIDTH:Number = 52;

	private var _model:BoardModel;

	private var _stones:Vector.<StoneView>;
	private var _territoryStones:Vector.<StoneView>;
	private var _cPoints:Vector.<CPointView>;
	private var _stoneSelector:Sprite;

	public function BoardView(model:BoardModel) {
		super();
		_model = model;
		init();
	}

	public function clear():void {
		for each (var stone:StoneView in _stones) {
			if (contains(stone)) { removeChild(stone); }
		}
		for each (var cPoint:CPointView in _cPoints) {
			if (contains(cPoint)) { removeChild(cPoint); }
		}
		_cPoints = null;
		_stones = null;
		trace("board cleared [BoardView.clear]");
	}

	public function setPassMove():void {
		if (_stones && _stones.length > 0) {
			var stone:StoneView = _stones[_stones.length-1];
			if (stone.contains(_stoneSelector)) {
				stone.removeChild(_stoneSelector);
			}
		}
	}

	public function addStone(stoneVO:StoneVO):void {
		var stoneView = createStone(stoneVO);
		if (!stoneVO.basic) {
			stoneView.addChild(_stoneSelector);
		}
		if (!_stones) { _stones = new Vector.<StoneView>(); }
		_stones.push(stoneView);
		addChild(stoneView);
	}

	public function removeStones(stoneVOs:Vector.<StoneVO>):void {
		for each (var stoneVO:StoneVO in stoneVOs) {
			removeStone(stoneVO);
		}
	}

	public function showHiddenStonesThenRemoveDeads(hiddenStones:Vector.<StoneVO>, deads:Vector.<StoneVO>):void {
		var timeline:TimelineMax = new TimelineMax({onComplete: function():void { removeStones(deads); }});
		var stoneView:StoneView;
		if (!_stones) { _stones = new Vector.<StoneView>(); }
		for each (var hiddenStone:StoneVO in hiddenStones) {
			if (!getStoneByVO(hiddenStone)) {
				stoneView = createStone(hiddenStone);
				_stones.push(stoneView);
				addChild(stoneView);
				stoneView.alpha = 0;
				timeline.insert(new TweenMax(stoneView, .6, {alpha: 1}));
			}
		}
		timeline.play();
	}

	public function justShowHiddenStone(hiddenStone:StoneVO):void {
		var stoneView:StoneView = createStone(hiddenStone);
		_stones.push(stoneView);
		addChild(stoneView);
		stoneView.alpha = 0;
		new TweenMax(stoneView, .6, {alpha: 1});
	}

	public function showTerritory(color:uint, points:Vector.<Point>):void {
		var stoneView:StoneView;
		for each (var point:Point in points) {
			stoneView = new StoneView(new StoneVO(color, point.x, point.y));
			stoneView.x = getBoardX(x);
			stoneView.y = getBoardY(y);
			stoneView.scaleX = stoneView.scaleY = .5;
			if (!_territoryStones) { _territoryStones = new Vector.<StoneView>(); }
			_territoryStones.push(stoneView);
			addChild(stoneView);
		}
	}
	public function cleanTerritory():void {
		for each (var stone:StoneView in _territoryStones) {
			if (contains(stone)) { removeChild(stone); }
		}
		_territoryStones = null;
	}

	public function showEnemyHiddenStoneEffect():void {
		var thisBoardView:BoardView = this;
		var timeline:TimelineMax = new TimelineMax({onComplete: function():void {thisBoardView.filters = []; }});
		timeline.append( new TweenMax(this, .6, {glowFilter:{color: 0x00BFFF, alpha : 1, blurX: 20, blurY: 20, strength: 5}}));
		timeline.append( new TweenMax(this, .6, {glowFilter:{color: 0x00BFFF, alpha : 0, blurX: 20, blurY: 20, strength: 5}}));
		timeline.play();
	}

	private function createStone(stoneVO:StoneVO):StoneView {
		var stoneView:StoneView = new StoneView(stoneVO);
		stoneView.x = getBoardX(stoneVO.x);
		stoneView.y = getBoardY(stoneVO.y);
		return stoneView;
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
		addChild(new Main.BOARD_VIEW());
		createStoneSelector();
		addCountablePoints();
		addListeners();
	}

	private function createStoneSelector():void {
		_stoneSelector = new Sprite();
		_stoneSelector.graphics.beginFill(0x8B2323);
		_stoneSelector.graphics.drawCircle(0, 0, 6);
		_stoneSelector.graphics.endFill();
	}

	private function addCountablePoints():void {
		var cPoints:Vector.<CountablePoint> = _model.getCountablePoints();
		if (cPoints) {
			for each (var cPoint:CountablePoint in cPoints) {
				addCPointToBoard(cPoint);
			}
		}
	}

	private function addCPointToBoard(countablePoint:CountablePoint):void {
		if (!_cPoints) { _cPoints = new Vector.<CPointView>(); }
		var cPointView:CPointView = new CPointView(countablePoint.count > 0);
		cPointView.x = getBoardX(countablePoint.x);
		cPointView.y = getBoardY(countablePoint.y);
		_cPoints.push(cPointView);
		super.addChild(cPointView);
	}

	private function addListeners():void {
		super.addEventListener(MouseEvent.CLICK, onMouseClick);
	}

	private function onMouseClick(event:MouseEvent):void {
		var stoneX:int = (super.mouseX + CELL_WIDTH/2 - BORDER_WIDTH) / CELL_WIDTH;
		var stoneY:int = (super.mouseY + CELL_HEIGHT/2 - BORDER_WIDTH) / CELL_HEIGHT;
		if (stoneX >= 0 && stoneX <= GameController.ROWS_NUM &&
				stoneY >= 0 && stoneY <= GameController.ROWS_NUM) {
			dispatchEvent(new BoardViewEvent(BoardViewEvent.CLICK, stoneX, stoneY, event.shiftKey));
		}
	}

	public function get boardWidth():Number {
		return (GameController.ROWS_NUM-1) * CELL_WIDTH + BORDER_WIDTH*2;
	}
	public function get boardHeight():Number {
		return (GameController.ROWS_NUM-1) * CELL_HEIGHT + BORDER_WIDTH*2;
	}

	private function getBoardX(matrixX:int):Number { return BORDER_WIDTH + matrixX * CELL_WIDTH; }
	private function getBoardY(matrixY:int):Number { return BORDER_WIDTH + matrixY * CELL_HEIGHT; }

}
}
