/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 8:44 PM
 */
package game {
import controller.GameController;

import game.stone.StoneVO;

public class GameModel {
	private var _matrix:Vector.<Vector.<StoneVO>>;
	private var _lastStoneVO:StoneVO;

	public function GameModel() {
		super();
		createMatrix();
	}

	public function get lastStoneVO():StoneVO { return _lastStoneVO; }

	public function canMove(x:int, y:int):Boolean {
		return _matrix[x][y] == null;
	}

	public function getNumStones():int {
		var result:int = 0;
		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameController.ROWS_NUM; ++j) {
				if (_matrix[i][j]) { result++; }
			}
		}
		return result;
	}

	public function addStone(stoneVO:StoneVO):void {
		if (!stoneVO) { return; }
		_lastStoneVO = stoneVO;
		_matrix[stoneVO.x][stoneVO.y] = stoneVO;
	}

	public function removeStones(stoneVOs:Vector.<StoneVO>):void {
		for each (var stoneVO:StoneVO in stoneVOs) {
			_matrix[stoneVO.x][stoneVO.y] = null;
		}
	}

	public function getDeadStones():Vector.<StoneVO> {
		return findDeadStones();
	}

	private function createMatrix():void {
		_matrix = new Vector.<Vector.<StoneVO>>(GameController.ROWS_NUM, true);
		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			_matrix[i] = new Vector.<StoneVO>(GameController.ROWS_NUM, true);
		}
	}

	private function findDeadStones():Vector.<StoneVO> {
		var result:Vector.<StoneVO> = new Vector.<StoneVO>();
		var x:int = _lastStoneVO.x;
		var y:int = _lastStoneVO.y;
		var loopX:int;
		var loopY:int;
		var deadStones:Vector.<StoneVO> = new Vector.<StoneVO>();
		for (var i:int = 0; i < 4; ++i) {
			loopX = x + ((i + 1) % 2) * (-1 + i);
			loopY = y + (i % 2) * (-2 + i);
			if (validPoint(loopX, loopY) && _matrix[loopX][loopY] && _matrix[loopX][loopY].color != _lastStoneVO.color) {
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
			if (validPoint(stoneVO.x-1, stoneVO.y)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x-1][stoneVO.y], deadStones, wasStones, finished);
			}
			if (validPoint(stoneVO.x, stoneVO.y-1)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x][stoneVO.y-1], deadStones, wasStones, finished);
			}
			if (validPoint(stoneVO.x+1, stoneVO.y)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x+1][stoneVO.y], deadStones, wasStones, finished);
			}
			if (validPoint(stoneVO.x, stoneVO.y+1)) {
				finished = findDeadStonesWithStone(color, _matrix[stoneVO.x][stoneVO.y+1], deadStones, wasStones, finished);
			}
		}
		return finished;
	}

	private function validPoint(x:int, y:int):Boolean {
		return x >=0 && x < GameController.ROWS_NUM && y >= 0 && y < GameController.ROWS_NUM;
	}
}
}
