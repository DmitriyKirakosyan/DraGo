/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 5/26/12
 * Time: 1:29 PM
 * To change this template use File | Settings | File Templates.
 */
package game.estimate {
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

	public function MatchEstimator(gameModel:GameModel):void {
		super();
		_gameModel = gameModel;
		_matrix = _gameModel.matrix;
	}

	public function whitePoints():Vector.<Point> { return _whitePoints; }
	public function blackPoints():Vector.<Point> { return _blackPoints; }


	private function estimate():void {
		_whitePoints = new Vector.<Point>();
		_blackPoints = new Vector.<Point>();
		var estimatingPoints:Vector.<Point> = new Vector.<Point>();

		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameController.ROWS_NUM; ++j) {
				findCapturedStone(null, new Point(i, j), estimatingPoints);
			}
		}
	}

	private function findCapturedStone(capturedByColor:uint, point:Point, capturedPoints:Vector.<Point>,
																					 wasPoins:Vector.<StoneVO> = new Vector.<StoneVO>(), finished:Boolean = false):Boolean {

		if (_matrix[point.x][point.y]) {
			if (capturedByColor && capturedByColor != _matrix[point.x][point.y].color) {
				capturedPoints.splice(0, capturedPoints.length);
				return true;
			} else { capturedByColor = _matrix[point.x][point.y].color; }
		} else {
			if ((wasPoins.indexOf(point) == -1) && !finished) {
				wasPoins.push(point);
				capturedPoints.push(point);

				if (validPoint(point.x-1, point.y)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x-1, point.y), capturedPoints, wasPoins, finished);
				}
				if (validPoint(point.x, point.y-1)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x, point.y-1), capturedPoints, wasPoins, finished);
				}
				if (validPoint(point.x+1, point.y)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x+1, point.y), capturedPoints, wasPoins, finished);
				}
				if (validPoint(point.x, point.y+1)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x, point.y+1), capturedPoints, wasPoins, finished);
				}
			}
		}
		return finished;
	}

	private function validPoint(x:int, y:int):Boolean {
		return x >=0 && x < GameController.ROWS_NUM && y >= 0 && y < GameController.ROWS_NUM;
	}

}
}
