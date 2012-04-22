package mainMenu {
import controller.ViewController;

import flash.display.Sprite;
import flash.text.TextField;
import flash.text.TextFieldAutoSize;
import flash.text.TextFormat;

public class Dashboard extends ViewController {
		private var _container:Sprite;
		
		private const CONTAINER_X:Number = 16;
		private const CONTAINER_Y:Number = 38;
		
		public function Dashboard(view:DashboardView) {
			super(view);
			addContainer();
			addText();
		}
		
		public function get board():Sprite { return _container; }
		
		private function addContainer():void {
			_container = new Sprite();
			_container.x = CONTAINER_X;
			_container.y = CONTAINER_Y;
			_container.mask = thisView.boardMask;
			view.addChild(_container);
		}

		private function addText():void {
			var textField:TextField = new TextField();
			var textFormat:TextFormat = new TextFormat(null, 16);
			textField.selectable = false;
			textField.width = 200;
			textField.wordWrap = true;
			textField.defaultTextFormat = textFormat;
			textField.text = "welcome to DraGo game, click start button to begin the game";
			_container.addChild(textField);
		}
		
		private function get thisView():DashboardView { return view as DashboardView; }
		
	}
}