package mainMenu {
	import flash.display.Sprite;

	public class MainMenu extends MenuView {
		private var _dashboard:Dashboard;
		
		public function MainMenu() {
			super();
			initObjects();
		}
		
		public function getDashboard():Dashboard { return _dashboard; }
		
		private function initObjects():void {
			_dashboard = new Dashboard(dashboard);
		}
		
	}
}