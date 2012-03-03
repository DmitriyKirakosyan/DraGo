package tests {
	import by.blooddy.crypto.serialization.JSON;
	
	import flash.display.Sprite;
	import flash.events.MouseEvent;
	import flash.text.TextField;
	import flash.text.TextFieldAutoSize;
	import flash.text.TextFieldType;
	
	import mainMenu.MainMenu;
	
	import rpc.GameRpc;

	/**
	 * @author dima
	 */
	public class RpcTest {
		private var _container:Sprite;
		
		private var tfNumber1:TextField;
		private var tfNumber2:TextField;
		private var tfResult:TextField;
		
		public function RpcTest(menu:MainMenu):void {
			_container = menu.getDashboard().board;
			drawInterface();
			sendToRpc();
		}
		
		private function drawInterface():void {
			tfNumber1 = new TextField();
			tfNumber1.autoSize = TextFieldAutoSize.LEFT;
			tfNumber1.text = "hi here";
			tfNumber1.x = 0; tfNumber1.y = 10;
			tfNumber2 = new TextField();
			tfNumber2.autoSize = TextFieldAutoSize.LEFT;
			tfNumber2.text = "hihi";
			tfNumber2.x = 0; tfNumber2.y = 20;
			_container.addChild(tfNumber1);
			_container.addChild(tfNumber2);
		}
		
		private function sendToRpc():void {
			//GameRpc.instance.init("127.0.0.1", 4444);
			//GameRpc.instance.send({num1:tfNumber1.text, num2:tfNumber2.text}, onResponse);
		}
		
		private function onResponse(data:String):void {
			
		}
	}
}
