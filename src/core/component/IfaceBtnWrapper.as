/**
 * User: dima
 * Date: 10/04/12
 * Time: 2:49 PM
 */
package core.component {
import flash.display.BlendMode;
import flash.display.Sprite;
import flash.events.MouseEvent;
import flash.filters.ColorMatrixFilter;

public class IfaceBtnWrapper {
	private var _btn:Sprite;

	private const COLORMATRIX_FILTER:ColorMatrixFilter = new ColorMatrixFilter();

	public function IfaceBtnWrapper(btn:Sprite):void {
		super();
		createColorMatrixFilter();
		_btn = btn;
		btn.addEventListener(MouseEvent.MOUSE_OVER, onBtnMouseOver);
		btn.addEventListener(MouseEvent.MOUSE_OUT, onBtnMouseOut);
	}

	public function get btn():Sprite { return _btn; }

	public static function wrap(btn:Sprite):void {
		new IfaceBtnWrapper(btn);
	}

	private function onBtnMouseOver(event:MouseEvent):void {
		_btn.filters = [COLORMATRIX_FILTER];
	}
	private function onBtnMouseOut(event:MouseEvent):void {
		_btn.filters = [];
	}

	private function createColorMatrixFilter():void {
		COLORMATRIX_FILTER.matrix = [1, 0, 0, 0.15, 0,
												0, 1, 0, 0.15, 0,
												0, 0, 1, 0.15, 0,
												0, 0, 0, 1, 0];
	}

}
}
