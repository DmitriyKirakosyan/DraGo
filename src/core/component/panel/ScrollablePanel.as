/**
 * User: dima
 * Date: 13/02/12
 * Time: 4:56 PM
 */
package core.component.panel {

import core.display.SceneSprite;

import flash.display.Sprite;

public class ScrollablePanel extends PanelBase {
	public var spacing:int = 8;
	
	public var scrollValue:int = 3;
	public var scrollSpeed:int = 1;
	
	public var currentIndex:int;

	public function ScrollablePanel(width:int, height:int) {
		super();
		_panelHeight = height;
		_panelWidth = width;
	}

	public function get panelWidth():int { return _panelWidth; }
	public function get panelHeight():int { return _panelHeight; }
	
	public function scrollRight():void{
		
	}
	
	public function scrollLeft():void{
		
	}
	
	protected function canScroll():Boolean{
		return true;
	}
}
}
