package {
import controller.GameController;

import flash.display.Sprite;
	
	import mainMenu.MainMenu;

import scene.SceneController;

import tests.RpcTest;
	
	[SWF(width=512, height=384, frameRate=25)]
	public class Main extends Sprite {
		public static const WIDTH:Number = 512;
		public static const HEIGHT:Number = 384;

		private var _sceneController:SceneController;
		
		public function Main() {
			addScenes();
		}

		private function addScenes():void {
			_sceneController = new SceneController();
			const menuScene:MainMenu = new MainMenu(this);
			const gameScene:GameController = new GameController(this);
			_sceneController.addScene(menuScene, true);
			_sceneController.addScene(gameScene);
			_sceneController.addSceneDependence(menuScene, gameScene, true);
		}
	}
}