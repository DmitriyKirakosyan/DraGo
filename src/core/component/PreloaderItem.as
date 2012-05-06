package core.component {
	
	import flash.display.Graphics;
	import flash.display.Sprite;
	import flash.events.Event;
	
	/**
	 * Класс крутящегося лепесткого прелоадера как на макос.
	 * 
	 * Начинает крутиться только когда добавляют на сцену, и перестает когда удаляют со сцены.
	 * 
	 * @author kutu
	 */
	public class PreloaderItem extends Sprite {
		
		private static const NUM_PARTS:uint = 12;
		private static const DEGREE_PER_PART:Number = 360 / NUM_PARTS;
		
		/**
		 * Конструктор создает крутящийся лепестковый прелоадер с параметрами
		 * 
		 * @param radius внешний радиус
		 * @param innerRadius внутренний радиус
		 * @param color цвет лепестков
		 * @param thickness толщина линии каждого лепестка
		 */
		public function PreloaderItem(radius:uint = 8, innerRadius:uint = 5, color:uint = 0x888888, thickness:uint = 2) {
			mouseEnabled = false;
			
			const g:Graphics = graphics;
			const rad:Number = Math.PI / 180;
			var angle:Number;
			
			for (var i:uint = 0; i < NUM_PARTS; ++i) {
				angle = i * DEGREE_PER_PART * rad;
				g.lineStyle(thickness, color, (i + 1) * (1 / (NUM_PARTS - 1)));
				g.moveTo(Math.cos(angle) * innerRadius, Math.sin(angle) * innerRadius);
				g.lineTo(Math.cos(angle) * radius, Math.sin(angle) * radius);
			}
			
			addEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
		}
		
		private function onAddedToStage(event:Event):void {
			removeEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
			addEventListener(Event.REMOVED_FROM_STAGE, onRemoveFromStage);
			addEventListener(Event.ENTER_FRAME, onLoop);
		}
		private function onRemoveFromStage(event:Event):void {
			removeEventListener(Event.REMOVED_FROM_STAGE, onRemoveFromStage);
			removeEventListener(Event.ENTER_FRAME, onLoop);
			addEventListener(Event.ADDED_TO_STAGE, onAddedToStage);
		}
		
		private function onLoop(event:Event):void {
			rotation += DEGREE_PER_PART;
		}
		
	}
	
}
