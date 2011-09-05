package {
	import flash.display.Sprite;
	
	import mainMenu.MainMenu;
	
	import tests.RpcTest;
	
	[SWF(width=512, height=384, frameRate=25)]
	public class DraGo extends Sprite {
		private var _menu:MainMenu;
		
		public function DraGo() {
			_menu = new MainMenu();
			this.addChild(_menu);
			new RpcTest(_menu);
		}
	}
}