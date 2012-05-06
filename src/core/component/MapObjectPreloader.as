package core.component {
	
	import as3isolib.enum.IsoOrientation;
	import as3isolib.geom.IsoMath;
	import as3isolib.geom.Pt;
	import as3isolib.utils.IsoDrawingUtil;
	
	import flash.display.Graphics;
	import flash.display.Shape;
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.geom.Matrix;
	import flash.geom.Point;
	
	public class MapObjectPreloader extends Sprite {
		
		public static const UNIT_SIZE:uint = 32;
		
		private const sh:Shape = new Shape();
		
		public function MapObjectPreloader(w:uint, l:uint, h:uint = 0, z:uint = 0) {
			const sp:Sprite = new Sprite();
			const p:Pt = IsoMath.isoToScreen(new Pt(w * UNIT_SIZE * .5, l * UNIT_SIZE * .5, (h * .5 + z) * UNIT_SIZE));
			
			var m:Matrix, min:uint;
			
			if (!w) {
				m = IsoDrawingUtil.getIsoMatrix(IsoOrientation.YZ);
				min = Math.min(l ? l : 1, h ? h : 1);
			} else if (!l) {
				m = IsoDrawingUtil.getIsoMatrix(IsoOrientation.XZ);
				min = Math.min(w ? w : 1, h ? h : 1);
			} else if (!h) {
				m = IsoDrawingUtil.getIsoMatrix(IsoOrientation.XY);
				min = Math.min(w ? w : 1, l ? l : 1);
			}
			
			m.tx = p.x;
			m.ty = p.y;
			
			sp.transform.matrix = m;
			
			const radius:uint = min * UNIT_SIZE >> 1;
			const innerRadius:uint = radius * .4 + .5;
			
			const g:Graphics = sh.graphics;
			g.beginFill(0, .1);
			drawCircleSector(g, 0, 0, radius, 150, innerRadius);
			
			sp.addChild(sh);
			addChild(sp);
			
			addEventListener(Event.ADDED_TO_STAGE, onAdded);
		}
		
		private function onLoop(event:Event):void {
			sh.rotation += scaleX > 0 ? 10 : -10;
		}
		
		private function onAdded(event:Event):void {
			if (!hasEventListener(Event.ENTER_FRAME)) {
				addEventListener(Event.ENTER_FRAME, onLoop);
				addEventListener(Event.REMOVED_FROM_STAGE, onRemove);
			}
		}
		private function onRemove(event:Event):void {
			removeEventListener(Event.ENTER_FRAME, onLoop);
			removeEventListener(Event.REMOVED_FROM_STAGE, onRemove);
		}
		
		private static function drawCircleSector(g:Graphics, centerX:Number, centerY:Number, radius:Number, angle:Number, innerRadius:Number = 0):void {
			var p:Point, i:int;
			const rad:Number = Math.PI / 180;
			const pi2:Number = Math.PI / 2;
			
			g.moveTo(centerX, 0);
			for (i = 0; i <= angle; ++i) {
				p = Point.polar(radius, i * rad - pi2);
				g.lineTo(centerX + p.x, centerY + p.y);
			}
			
			if (innerRadius != 0 && innerRadius < radius) {
				for (i = angle; i >= 0; --i) {
					p = Point.polar(innerRadius, i * rad - pi2);
					g.lineTo(centerX + p.x, centerY + p.y);
				}
			}
		}
		
	}
	
}
