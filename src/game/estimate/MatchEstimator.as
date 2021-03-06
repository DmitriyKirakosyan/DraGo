/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 5/26/12
 * Time: 1:29 PM
 * To change this template use File | Settings | File Templates.
 */
package game.estimate {

import controller.GameProcess;

import flash.geom.Point;

import game.board.model.BoardModel;

import game.stone.StoneVO;

public class MatchEstimator {
	private var _gameModel:BoardModel;
	private var _matrix:Vector.<Vector.<StoneVO>>;
	private var _whitePoints:Vector.<Point>;
	private var _blackPoints:Vector.<Point>;
	private var _whiteTerritoryCounts:int;
	private var _blackTerritoryCounts:int;

	//очки, зарабатываемые во время игры (пленники, определенные точки доски и тп
	private var _whiteBaseCounts:int;
	private var _blackBaseCounts:int;

	private var _capturedStonesCollection:Vector.<Vector.<StoneVO>>;

	private const WHITE:String = "white";
	private const BLACK:String = "black";
	private const NONE:String = "none";

	public function MatchEstimator(gameModel:BoardModel):void {
		super();
		_whiteTerritoryCounts = 0;
		_blackTerritoryCounts = 0;
		_whiteBaseCounts = 0;
		_blackBaseCounts = 0;
		_gameModel = gameModel;
		_matrix = _gameModel.matrix;
		_capturedStonesCollection = new Vector.<Vector.<StoneVO>>();
	}

	public function clean():void {
		_whiteBaseCounts = 0;
		_blackBaseCounts = 0;
	}

	public function addWhiteCounts(value:int):void {
		_whiteBaseCounts += value;
	}
	public function addBlackCounts(value:int):void {
		_blackBaseCounts += value;
	}

	public function get whitePoints():Vector.<Point> { return _whitePoints; }
	public function get blackPoints():Vector.<Point> { return _blackPoints; }
	public function get whiteCounts():int { return _whiteTerritoryCounts + _whiteBaseCounts; }
	public function get blackCounts():int { return _blackTerritoryCounts + _blackBaseCounts; }

	public function hasCapturedPoint(point:Point):Boolean {
		var stone:StoneVO = _matrix[point.x][point.y];
		return stone && isCapturedStone(stone);
	}

	public function getCapturedPoints(point:Point):Vector.<StoneVO> {
		var stone:StoneVO = _matrix[point.x][point.y];
		if (!stone) { return null; }
		for each (var capturedStones:Vector.<StoneVO> in _capturedStonesCollection) {
			for each (var capturedStone:StoneVO in capturedStones) {
				if (stone == capturedStone) {
					return capturedStones;
				}
			}
		}
		return null;
	}

	public function addCapturedStone(point:Point):void {
		var stone:StoneVO = _matrix[point.x][point.y];
		if (!stone) { return; }
		if (!isCapturedStone(stone)) {
			var capturedStones:Vector.<StoneVO> = new Vector.<StoneVO>();
			findCapturedStones(stone.color, new Point(stone.x, stone.y), capturedStones, new Vector.<Point>());
			_capturedStonesCollection.push(capturedStones);
		}
		trace("captured stones length : " + capturedStones.length + " [MatchEstimator.addCapturedStone]");
	}
	public function removeCapturedStone(point:Point):void {
		var stone:StoneVO = _matrix[point.x][point.y];
		var index:int = -1;
		for (var i:int = 0; i < _capturedStonesCollection.length; ++i) {
			if (_capturedStonesCollection[i].indexOf(stone) != -1) {
				index = i;
				break;
			}
		}
		if (index != -1) {
			_capturedStonesCollection.splice(index, 1);
		}
	}


	public function estimate():void {
		_whitePoints = new Vector.<Point>();
		_blackPoints = new Vector.<Point>();
		var wasPoints:Vector.<Point> = new Vector.<Point>();
		var estimatingPoints:Vector.<Point> = new Vector.<Point>();
		var capturedByColor:Object;

		for (var i:int = 0; i < GameProcess.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameProcess.ROWS_NUM; ++j) {
				capturedByColor = {};
				findCapturedPoints(capturedByColor, new Point(i, j), estimatingPoints, wasPoints);
				if (estimatingPoints.length > 0) {
					for each (var point:Point in estimatingPoints) {
						if (capturedByColor["color"] == WHITE) {
							_whitePoints.push(point);
						} else if (capturedByColor["color"] == BLACK) {
							_blackPoints.push(point);
						}
						removeStonesPointsFrom(wasPoints);
						//wasPoints.push(point);
					}
					estimatingPoints = new Vector.<Point>();
				}
			}
		}
		updatePlayersCounts();
	}


	/* Internal functions */


	private function updatePlayersCounts():void {
		_whiteTerritoryCounts = 0;
		for each (var whitePoint:Point in _whitePoints) {
			_whiteTerritoryCounts += _matrix[whitePoint.x][whitePoint.y] ? 2 : 1;
		}
		_blackTerritoryCounts = 0;
		for each (var blackPoint:Point in _blackPoints) {
			_blackTerritoryCounts += _matrix[blackPoint.x][blackPoint.y] ? 2 : 1;
		}
	}

	private function findCapturedPoints(colorContainer:Object, point:Point, capturedPoints:Vector.<Point>,
																					 wasPoints:Vector.<Point>):void {
		if (hasPointInVector(point.x, point.y, wasPoints)) {
			return;
		} else {
			wasPoints.push(point);
		}
		if (_matrix[point.x][point.y] && !isCapturedStone(_matrix[point.x][point.y])) {
			if (colorContainer["color"] && colorContainer["color"] != NONE) {
				var color:uint = colorContainer["color"] == "white" ? StoneVO.WHITE : StoneVO.BLACK;
				if (color != _matrix[point.x][point.y].color) {
					colorContainer["color"] = NONE;
				}
			} else if (colorContainer["color"] != NONE) { colorContainer["color"] = _matrix[point.x][point.y].color == StoneVO.WHITE ? WHITE : BLACK; }
		} else {
			capturedPoints.push(point);

			if (validPoint(point.x-1, point.y)) {
				findCapturedPoints(colorContainer, new Point(point.x-1, point.y), capturedPoints, wasPoints);
			}
			if (validPoint(point.x, point.y-1)) {
				findCapturedPoints(colorContainer, new Point(point.x, point.y-1), capturedPoints, wasPoints);
			}
			if (validPoint(point.x+1, point.y)) {
				findCapturedPoints(colorContainer, new Point(point.x+1, point.y), capturedPoints, wasPoints);
			}
			if (validPoint(point.x, point.y+1)) {
				findCapturedPoints(colorContainer, new Point(point.x, point.y+1), capturedPoints, wasPoints);
			}
		}
	}

	private function findCapturedStones(color:uint, point:Point, capturedStones:Vector.<StoneVO>, wasPoints:Vector.<Point>):void {
		if (hasPointInVector(point.x, point.y, wasPoints)) {
			return;
		} else {
			wasPoints.push(point);
		}
		if (_matrix[point.x][point.y]) {
			if (color == _matrix[point.x][point.y].color) {
				capturedStones.push(_matrix[point.x][point.y]);
			} else {
				return;
			}
		}
		if (validPoint(point.x-1, point.y)) {
			findCapturedStones(color, new Point(point.x-1, point.y), capturedStones, wasPoints);
		}
		if (validPoint(point.x, point.y-1)) {
			findCapturedStones(color, new Point(point.x, point.y-1), capturedStones, wasPoints);
		}
		if (validPoint(point.x+1, point.y)) {
			findCapturedStones(color, new Point(point.x+1, point.y), capturedStones, wasPoints);
		}
		if (validPoint(point.x, point.y+1)) {
			findCapturedStones(color, new Point(point.x, point.y+1), capturedStones, wasPoints);
		}
	}

	private function isCapturedStone(stone:StoneVO):Boolean {
		for each (var capturedStones:Vector.<StoneVO> in _capturedStonesCollection) {
			for each (var capturedStone:StoneVO in capturedStones) {
				if (stone == capturedStone) {
					return true;
				}
			}
		}
		return false;
	}

	private function removeStonesPointsFrom(points:Vector.<Point>):void {
		var pointsForRemove:Vector.<Point> = new Vector.<Point>();
		for each (var point:Point in points) {
			if (_matrix[point.x][point.y]) {
				pointsForRemove.push(point);
			}
		}
		var index:int;
		for each (var pointForRemove:Point in pointsForRemove) {
			index = points.indexOf(pointForRemove);
			points.splice(index, 1);
		}
	}

	private function hasPointInVector(x:int, y:int, vector:Vector.<Point>):Boolean {
		for each (var point:Point in vector) {
			if (x == point.x && y == point.y) {
				return true;
			}
		}
		return false;
	}

	private function validPoint(x:int, y:int):Boolean {
		return x >=0 && x < GameProcess.ROWS_NUM && y >= 0 && y < GameProcess.ROWS_NUM;
	}

}
}
