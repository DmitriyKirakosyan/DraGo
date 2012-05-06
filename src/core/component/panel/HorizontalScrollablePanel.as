/**
 * User: dima
 * Date: 16/02/12
 * Time: 12:49 PM
 */
package core.component.panel {
import com.greensock.TweenMax;

import core.display.SceneSprite;

import flash.display.Sprite;

public class HorizontalScrollablePanel extends ScrollablePanel{
	
	protected var scrolling:Boolean;
	
	protected var paddingLeft:int = 40;
	
	public function HorizontalScrollablePanel(width:Number, height:Number) {
		super(width, height);
		drawBackgound();
	}
	
	protected function drawBackgound():void {
		graphics.beginFill(0x888888);
		graphics.drawRect(0,0,_panelWidth, _panelHeight);
		graphics.endFill();
	}
	
	override public function addItem(item:Sprite):void{
		super.addItem(item);
		verticalCenter(item);
		item.x = 40 + ((spacing + item.width) * (items.length-1));
	}
	
	private function verticalCenter(item:Sprite):void{
		item.y = panelHeight/2 - item.height/2;
	}

	
	override public function scrollLeft():void{
		if(canScroll()){
			scrolling = true;
			addNewVisibleItemsToTheRight();
			currentIndex += scrollValue;
			if(currentIndex > items.length){
				currentIndex = items.length;
			}
			for(var i:int; i < _shownItems.length; i++){
				TweenMax.to(_shownItems[i], scrollSpeed, {"x":_shownItems[i].x - ((_shownItems[i].width + spacing) * scrollValue), onComplete:function():void{
					scrolling = false;
					removeInvisibleItems();
				}});
			}
		}
	}
	
	override public function scrollRight():void{
		if(canScroll() && (currentIndex - scrollValue) >= 0){
			scrolling = true;
			addNewVisibleItemsToTheLeft();
			currentIndex -= scrollValue;
			if(currentIndex < 0){
				currentIndex = 0;
			}
			for(var i:int = 0; i < _shownItems.length; i++){
				TweenMax.to(_shownItems[i], scrollSpeed, {"x":_shownItems[i].x + ((_shownItems[i].width + spacing) * scrollValue), onComplete:function():void{
					scrolling = false;
					removeInvisibleItems();
				}});
			}
		}
	}
	
	protected function addNewVisibleItemsToTheLeft():void{
		var k:int = 0;
		for(var i:int = currentIndex - scrollValue; i < currentIndex; i++){
			_shownItems.push(items[i]);
			itemsContainer.addChild(items[i]);
			items[i].x = (items[i].width + spacing) * k - ((items[i].width + spacing) * scrollValue) + paddingLeft;
			k++;
		}
	}
	
	protected function addNewVisibleItemsToTheRight():void{
		var k:int = 0;
		var lastShownEl:int = currentIndex + _shownItems.length;
		for(var i:int = lastShownEl; i < items.length; i++){
			_shownItems.push(items[i]);
			itemsContainer.addChild(items[i]);
			items[i].x = (items[i].width + spacing) * k + ((items[i].width + spacing) * _shownItems.length);
			k++;
		}
	}
	
	protected function removeInvisibleItems():void{
		var tempVector:Vector.<Sprite> = new Vector.<Sprite>;
		for (var i:int; i<_shownItems.length; i++){
			if((_shownItems[i].x > panelWidth) || (_shownItems[i].x < 0)){
				if(itemsContainer.contains(_shownItems[i])){
					itemsContainer.removeChild(_shownItems[i]);
				}
			}else{
				tempVector.push(_shownItems[i]);
			}
		}
		_shownItems = tempVector;
	}
	
	override protected function canScroll():Boolean{
		return !scrolling;
	}
}
}
