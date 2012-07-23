/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 8:44 PM
 */
package game.board.model {

import com.bit101.components.InputText;

import controller.GameProcess;

import game.stone.StoneVO;

public class BoardModel {
	private var _matrix:Vector.<Vector.<StoneVO>>;
	private var _lastStoneVO:StoneVO;

	/**
	 * Для алгоритма поиска мертвых комней
	 */
	private var _hiddenStones:Vector.<StoneVO>;

	private var _countablePoints:Vector.<CountablePoint>;
	private static const NORMAL_COUNTABLE_POINT:int = 5;

	public static function standartBoard():BoardModel {
		var result:BoardModel = new BoardModel();
		result.addCountablePoints(basicCountablePoints());
		return result;
	}

	private static function basicCountablePoints():Vector.<CountablePoint> {
		var result:Vector.<CountablePoint> = new Vector.<CountablePoint>();
		result.push(new CountablePoint(5, 0, NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(5, 10, NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(0, 5, NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(10, 5, NORMAL_COUNTABLE_POINT));

		result.push(new CountablePoint(2, 2, -NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(2, 8, -NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(8, 8, -NORMAL_COUNTABLE_POINT));
		result.push(new CountablePoint(8, 2, -NORMAL_COUNTABLE_POINT));
		return result;
	}

	public function BoardModel() {
		super();
		createMatrix();
		_hiddenStones = new Vector.<StoneVO>();
	}

	public function addCountablePoints(cPoints:Vector.<CountablePoint>):void {
		for each (var point:CountablePoint in cPoints) {
			addCountablePoint(point.x, point.y, point.count);
		}
	}

	public function addCountablePoint(x:int, y:int, count:int):void {
		if (!_countablePoints) { _countablePoints = new Vector.<CountablePoint>(); }
		if (!countablePointExists(x, y)) {
			_countablePoints.push(new CountablePoint(x, y, count));
		} else {
			trace("already exists countable point [BoardModel.addCountablePoint]");
		}
	}

	public function getCountOfPoint(x:int, y:int):int {
		if (countablePointExists(x, y)) {
			return getCountablePoint(x, y).count;
		}
		return 0;
	}

	private function getCountablePoint(x:int, y:int):CountablePoint {
		for each (var cPoint:CountablePoint in _countablePoints) {
			if (cPoint.x == x && cPoint.y == y) { return cPoint; }
		}
		return null;
	}

	private function countablePointExists(x:int, y:int):Boolean {
		for each (var cPoint:CountablePoint in _countablePoints) {
			if (cPoint.x == x && cPoint.y == y) { return true; }
		}
		return false;
	}

	public function getCountablePoints():Vector.<CountablePoint> {
		return _countablePoints;
	}

	public function get lastStoneVO():StoneVO { return _lastStoneVO; }

	public function cleanHiddenStones():void {
		_hiddenStones = new Vector.<StoneVO>();
	}

	/**
	 * Найденные в ходе алгоритма окружения скрытые камни
	 */
	public function get hiddenStones():Vector.<StoneVO> { return _hiddenStones; }
	public function get matrix():Vector.<Vector.<StoneVO>> { return _matrix; }

	public function emptyPoint(x:int, y:int):Boolean {
		return _matrix[x][y] == null;
	}

	public function getNumStones():int {
		var result:int = 0;
		for (var i:int = 0; i < GameProcess.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameProcess.ROWS_NUM; ++j) {
				if (_matrix[i][j]) { result++; }
			}
		}
		return result;
	}

	public function addPass(stoneVO:StoneVO):void {
		_lastStoneVO = stoneVO;
	}

	public function getStone(x:int, y:int):StoneVO {
		if (isValidPoint(x, y)) {
			return _matrix[x][y];
		}
		return null;
	}

	public function addStone(stoneVO:StoneVO):void {
		if (!stoneVO) { return; }
		if (!stoneVO.basic) { _lastStoneVO = stoneVO; }
		_matrix[stoneVO.x][stoneVO.y] = stoneVO;
	}

	public function removeStones(stoneVOs:Vector.<StoneVO>):void {
		for each (var stoneVO:StoneVO in stoneVOs) {
			_matrix[stoneVO.x][stoneVO.y] = null;
		}
	}

	public function removeAllStones():void {
		for (var i:int = 0; i < GameProcess.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameProcess.ROWS_NUM; ++j) {
				_matrix[i][j] = null;
			}
		}
	}

	public function getDeadStones():Vector.<StoneVO> {
		return findDeadStones();
	}

	public function isSelfKilled(stoneVO:StoneVO):Boolean {
		if (_matrix[stoneVO.x][stoneVO.y]) { return false; }
		var deadStones:Vector.<StoneVO> = new Vector.<StoneVO>();
		_matrix[stoneVO.x][stoneVO.y] = stoneVO;
		findDeadStonesWithStone(stoneVO.color, stoneVO, deadStones);
		_matrix[stoneVO.x][stoneVO.y] = null;
		return deadStones.length > 0;
	}

	public function isValidPoint(x:int, y:int):Boolean {
		return x >=0 && x < GameProcess.ROWS_NUM && y >= 0 && y < GameProcess.ROWS_NUM;
	}

	// Internal functions

	private function createMatrix():void {
		_matrix = new Vector.<Vector.<StoneVO>>(GameProcess.ROWS_NUM, true);
		for (var i:int = 0; i < GameProcess.ROWS_NUM; ++i) {
			_matrix[i] = new Vector.<StoneVO>(GameProcess.ROWS_NUM, true);
		}
	}

	private function findDeadStones():Vector.<StoneVO> {
		var result:Vector.<StoneVO> = new Vector.<StoneVO>();
		if (!_lastStoneVO) { return result; }
		var x:int = _lastStoneVO.x;
		var y:int = _lastStoneVO.y;
		var loopX:int;
		var loopY:int;
		var deadStones:Vector.<StoneVO> = new Vector.<StoneVO>();
		for (var i:int = 0; i < 4; ++i) {
			loopX = x + ((i + 1) % 2) * (-1 + i);
			loopY = y + (i % 2) * (-2 + i);
			if (isValidPoint(loopX, loopY) && _matrix[loopX][loopY] && _matrix[loopX][loopY].color != _lastStoneVO.color) {
				findDeadStonesWithStone(_matrix[loopX][loopY].color, _matrix[loopX][loopY], deadStones);
				if (deadStones && deadStones.length > 0) {
					result = result.concat(deadStones);
					//return deadStones;
				}
			}
		}
		return result;
	}

	private function findDeadStonesWithStone(color:uint, stoneVO:StoneVO, deadStones:Vector.<StoneVO>,
																					 wasStones:Vector.<StoneVO> = null, finished:Boolean = false):Boolean {
		if (!stoneVO) {
			deadStones.splice(0, deadStones.length);
			return true;
		}
		if ((!wasStones || wasStones.indexOf(stoneVO) == -1) && stoneVO.color == color && !finished) {
			if (!wasStones) { wasStones = new Vector.<StoneVO>(); }
			wasStones.push(stoneVO);

			deadStones.push(stoneVO);
			if (isValidPoint(stoneVO.x-1, stoneVO.y)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x-1][stoneVO.y], deadStones, wasStones, finished);
			}
			if (isValidPoint(stoneVO.x, stoneVO.y-1)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x][stoneVO.y-1], deadStones, wasStones, finished);
			}
			if (isValidPoint(stoneVO.x+1, stoneVO.y)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x+1][stoneVO.y], deadStones, wasStones, finished);
			}
			if (isValidPoint(stoneVO.x, stoneVO.y+1)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x][stoneVO.y+1], deadStones, wasStones, finished);
			}
		} else if (stoneVO.color != color && stoneVO.hidden) {
			_hiddenStones.push(stoneVO);
		}
		return finished;
	}

}
}
