package core.component.panel
{
	import com.bit101.components.Label;

import core.display.AssetManager;

import core.display.AssetManager;
	import core.display.InteractivePNG;
	import core.display.SceneSprite;
	import core.event.AssetEvent;
	
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mouse.MouseManager;
	
	import ru.beenza.framework.components.PreloaderItem;

	/**
	 * PanelItem
	 * @author satansdeer
	 */
	public class PanelItem extends Sprite{

		private var _farm:Boolean;
		private var nameLabel:Label;
		private var _vo:Object;
		
		private var _preloader:PreloaderItem;
		
		public function PanelItem(vo:Object, farm:Boolean = false)
		{
			_vo = vo;
			_farm = farm;
			//drawBackground();
			//nameLabel = new Label(this, 4 , 60)
			//if(_vo.hasOwnProperty("name")){
			//	nameLabel.text = _vo.name;
			//}
			drawPreloader();
			if (AssetManager.existInCache(_vo.url)) {
				addImage();
			} else {
				AssetManager.instance.addEventListener(AssetEvent.ASSET_LOADED, onAssetLoaded);
				AssetManager.load(_vo.url);
			}
			addEventListener(Event.ADDED_TO_STAGE, onAdded);
			addEventListener(Event.REMOVED_FROM_STAGE, onRemoved);
		}
		
		protected function onRemoved(event:Event):void{
			removeListeners();
		}
		
		private function removeListeners():void {
//			removeEventListener(MouseEvent.MOUSE_OVER, onMouseOver);
//			removeEventListener(MouseEvent.MOUSE_OUT, onMouseOut);
			removeEventListener(MouseEvent.MOUSE_DOWN, onMouseDown);
			removeEventListener(MouseEvent.MOUSE_UP, onMouseUp);
		}
		
		protected function onAdded(event:Event):void{
			addListeners();
		}
		
		private function addListeners():void {
			//addEventListener(MouseEvent.MOUSE_OVER, onMouseOver);
			//addEventListener(MouseEvent.MOUSE_OUT, onMouseOut);
			//addEventListener(MouseEvent.MOUSE_DOWN, onMouseDown);
			//addEventListener(MouseEvent.MOUSE_UP, onMouseUp);
		}
		
		protected function onMouseOut(event:MouseEvent):void{
			width = 50;
			height = 80;
			x += 2;
			y +=2;
		}
		
		protected function onMouseOver(event:MouseEvent):void{
			width = 54;
			height = 84;
			x -= 2;
			y -= 2;
		}
		
		protected function onMouseUp(event:MouseEvent):void{
			if(MouseManager.instance.moved){
				MouseManager.instance.img = null;
			}
		}
		
		protected function onMouseDown(event:MouseEvent):void{
			MouseManager.instance.mode = MouseManager.NORMAL_MODE;
			MouseManager.instance.img = new InteractivePNG(AssetManager.getImageByURL(_vo.url))
			MouseManager.instance.data = _vo;
		}
		
		protected function onAssetLoaded(event:AssetEvent):void{
			if(event.url == _vo.url){
				addImage();
			}
		}

		private function addImage() {
			AssetManager.instance.removeEventListener(AssetEvent.ASSET_LOADED, onAssetLoaded);
			var img:InteractivePNG = new InteractivePNG(AssetManager.getImageByURL(_vo.url));
			if (!_farm) {
				img.width = 100;
				img.height = 100;
			} else {

			}
			img.x = _farm ? -300 : -50;
			img.y = _farm ? -280 : -50;
			img.mouseEnabled = false;
			addChild(img);
			removeChild(_preloader);
		}

		public function set itemName(value:String):void{
			nameLabel.text = value;
		}
		
		protected function drawPreloader():void{
			_preloader = new PreloaderItem();
			_preloader.x = width/2;
			_preloader.y = height/2;
			addChild(_preloader);
		}
		
		private function resizeImg(img:InteractivePNG):void{
			 var k:Number = img.width/width;
			 img.width = width;
			 img.height /= k;
		}
	}
}