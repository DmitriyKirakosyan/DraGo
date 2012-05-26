/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 5/26/12
 * Time: 1:29 PM
 * To change this template use File | Settings | File Templates.
 */
package game.estimate {
import avmplus.finish;

import game.*;

import controller.GameController;

import flash.geom.Point;

import game.stone.StoneVO;

public class MatchEstimator {
	private var _gameModel:GameModel;
	private var _matrix:Vector.<Vector.<StoneVO>>;
	private var _whitePoints:Vector.<Point>;
	private var _blackPoints:Vector.<Point>;

	private const WHITE:String = "white";
	private const BLACK:String = "black";
	private const NONE:String = "none";

	public function MatchEstimator(gameModel:GameModel):void {
		super();
		_gameModel = gameModel;
		_matrix = _gameModel.matrix;
	}

	public function whitePoints():Vector.<Point> { return _whitePoints; }
	public function blackPoints():Vector.<Point> { return _blackPoints; }


	public function estimate():void {
		_whitePoints = new Vector.<Point>();
		_blackPoints = new Vector.<Point>();
		var wasPoints:Vector.<Point> = new Vector.<Point>();
		var estimatingPoints:Vector.<Point> = new Vector.<Point>();
		var capturedByColor:Object;

		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameController.ROWS_NUM; ++j) {
				capturedByColor = {};
				findCapturedStone(capturedByColor, new Point(i, j), estimatingPoints, wasPoints);
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
	}

	private function findCapturedStone(colorContainer:Object, point:Point, capturedPoints:Vector.<Point>,
																					 wasPoints:Vector.<Point>, finished:Boolean = false):Boolean {
		if (hasPointInVector(point.x, point.y, wasPoints)) {
			return finished;
		} else {
			wasPoints.push(point);
		}
		if (_matrix[point.x][point.y]) {
			if (colorContainer["color"] && colorContainer["color"] != NONE) {
				var color:uint = colorContainer["color"] == "white" ? StoneVO.WHITE : StoneVO.BLACK;
				if (color != _matrix[point.x][point.y].color) {
					colorContainer["color"] = NONE;
				}
			} else if (colorContainer["color"] != NONE) { colorContainer["color"] = _matrix[point.x][point.y].color == StoneVO.WHITE ? WHITE : BLACK; }
		} else {
			if (!finished) {
				capturedPoints.push(point);

				if (validPoint(point.x-1, point.y)) {
					finished = findCapturedStone(colorContainer, new Point(point.x-1, point.y), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x, point.y-1)) {
					finished = findCapturedStone(colorContainer, new Point(point.x, point.y-1), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x+1, point.y)) {
					finished = findCapturedStone(colorContainer, new Point(point.x+1, point.y), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x, point.y+1)) {
					finished = findCapturedStone(colorContainer, new Point(point.x, point.y+1), capturedPoints, wasPoints, finished);
				}
			}
		}
		return finished;
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
		return x >=0 && x < GameController.ROWS_NUM && y >= 0 && y < GameController.ROWS_NUM;
	}

}
}
