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
		var wasPoints:Vector.<Point> = new Vector.<Point>();
		var estimatingPoints:Vector.<Point> = new Vector.<Point>();
		var capturedByColor:String;

		for (var i:int = 0; i < GameController.ROWS_NUM; ++i) {
			for (var j:int = 0; j < GameController.ROWS_NUM; ++j) {
				capturedByColor = "";
				findCapturedStone(capturedByColor, new Point(i, j), estimatingPoints, wasPoints);
				if (estimatingPoints.length > 0) {
					for each (var point:Point in estimatingPoints) {
						if (capturedByColor == WHITE) {
							_whitePoints.push(point);
						} else if (capturedByColor == BLACK) {
							_blackPoints.push(point);
						}
						wasPoints.push(point);
					}
					estimatingPoints = new Vector.<Point>();
				}
			}
		}
	}

	private function findCapturedStone(capturedByColor:String, point:Point, capturedPoints:Vector.<Point>,
																					 wasPoints:Vector.<Point> = new Vector.<Point>(), finished:Boolean = false):Boolean {

		if (_matrix[point.x][point.y]) {
			if (capturedByColor != "") {
				var color:uint = capturedByColor == "white" ? StoneVO.WHITE : StoneVO.BLACK;
				if (color != _matrix[point.x][point.y].color) {
					//capturedPoints.splice(0, capturedPoints.length);
					capturedByColor = "";
					return true;
				}
			} else { capturedByColor = _matrix[point.x][point.y].color == StoneVO.WHITE ? WHITE : BLACK; }
		} else {
			if ((wasPoints.indexOf(point) == -1) && !finished) {
				wasPoints.push(point);
				capturedPoints.push(point);

				if (validPoint(point.x-1, point.y)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x-1, point.y), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x, point.y-1)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x, point.y-1), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x+1, point.y)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x+1, point.y), capturedPoints, wasPoints, finished);
				}
				if (validPoint(point.x, point.y+1)) {
					finished = findCapturedStone(capturedByColor, new Point(point.x, point.y+1), capturedPoints, wasPoints, finished);
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
