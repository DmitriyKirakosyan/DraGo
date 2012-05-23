/**
 * Created by : Dmitry
 * Date: 4/26/12
 * Time: 10:24 PM
 */
package game.stone {
import flash.display.Sprite;
import flash.filters.GlowFilter;

import game.BoardView;

public class StoneView extends Sprite {
	private var _stoneVO:StoneVO;

	private const RADIUS:int = BoardView.CELL_WIDTH/2 - 4;

	public function StoneView(stoneVO:StoneVO) {
		super();
		_stoneVO = stoneVO;
		init();
		mouseEnabled = false;
	}

	public function get vo():StoneVO { return _stoneVO; }

	private function init():void {
		var color:uint = (_stoneVO.color == StoneVO.BLACK) ? 0 : 0xffffff;
		this.graphics.beginFill(color);
		this.graphics.drawCircle(0, 0, RADIUS);
		if (_stoneVO.basic) {
			this.graphics.drawCircle(0, 0, RADIUS/2);
		} else if (_stoneVO.hidden) {
			this.alpha = .4;
		}
		this.graphics.endFill();
		addFilter(color);
	}

	private function addFilter(color:uint):void {
		this.filters = [new GlowFilter(color)];
	}
}
}
