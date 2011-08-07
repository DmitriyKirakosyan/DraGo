package mainMenu {
	import flash.display.Sprite;

	public class Dashboard extends ViewController {
		private var _container:Sprite;
		
		private const CONTAINER_X:Number = 16;
		private const CONTAINER_Y:Number = 38;
		
		public function Dashboard(view:DashboardView) {
			super(view);
			addContainer();
		}
		
		public function get board():Sprite { return _container; }
		
		private function addContainer():void {
			_container = new Sprite();
			_container.x = CONTAINER_X;
			_container.y = CONTAINER_Y;
			_container.mask = thisView.boardMask;
			view.addChild(_container);
		}
		
		private function get thisView():DashboardView { return view as DashboardView; }
		
	}
}