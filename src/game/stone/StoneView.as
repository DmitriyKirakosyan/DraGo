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
		init((stoneVO.color == StoneVO.BLACK) ? 0 : 0xffffff);
		mouseEnabled = false;
	}

	public function get vo():StoneVO { return _stoneVO; }

	private function init(color:uint):void {
		this.graphics.beginFill(color);
		this.graphics.drawCircle(0, 0, RADIUS);
		this.graphics.endFill();
		addFilter(color);
	}

	private function addFilter(color:uint):void {
		this.filters = [new GlowFilter(color)];
	}
}
}
