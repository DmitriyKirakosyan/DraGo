/**
 * User: dima
 * Date: 13/02/12
 * Time: 4:52 PM
 */
package core.component.panel {
import core.display.SceneSprite;
import core.layer.LayersENUM;

import flash.display.Sprite;

public class PanelBase extends SceneSprite {
	protected var items:Vector.<Sprite>;

	protected var itemsMask:Sprite;
	
	protected var itemsContainer:Sprite = new Sprite();
	
	protected var _panelWidth:int;
	protected var _panelHeight:int;
	protected var _shownItems:Vector.<Sprite>;
	
	public function PanelBase():void {
		super(LayersENUM.INTERFACE);
		addChild(itemsContainer);
		//itemsContainer.mask = itemsMask;
	}

	public function addItem(item:Sprite):void {
		if (!items) { items = new Vector.<Sprite>(); }
		if (items.indexOf(item) == -1) { items.push(item); }
		if(isVisible(item)){
			itemsContainer.addChild(item);
			if (!_shownItems) { _shownItems = new Vector.<Sprite>(); }
			if (_shownItems.indexOf(item) == -1) { _shownItems.push(item); }
		}
	}

	public function removeItem(item:Sprite):void {
		if (contains(item)) {
			removeChild(item);
			var index:int = items.indexOf(item);
			if (index >= 0) { items.splice(index, 1); }
		}
	}
	
	protected function drawMask():void{
		
	}
	
	protected function isVisible(item:Sprite):Boolean{
		return true
	} 
}
}
