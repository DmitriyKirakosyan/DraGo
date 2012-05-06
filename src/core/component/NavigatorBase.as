package core.component {

import core.component.panel.PanelBase;
import flash.display.DisplayObject;
import flash.display.Shape;
import flash.display.Sprite;
import flash.events.Event;
import flash.geom.Rectangle;

public class NavigatorBase extends PanelBase {
		
		public static const FORCE_CHECK_LIMITS:String = "forceCheckLimits";
		
		public var defaultItemWidth:uint;
		public var defaultItemHeight:uint;
		public var horizontalGap:int = 5;
		public var verticalGap:int = 5;
		public var numColumns:uint = 7;
		public var numRows:uint = 1;
		public var extraPaddingForMask:Rectangle;
		public var debugMask:Boolean;
		
		protected var preloaderY:uint;
		
		protected var container:Sprite;
		protected var containerMask:Shape;
		protected var children:Vector.<DisplayObject>;
		protected var renderedChildren:Vector.<DisplayObject>;
		protected var preloaderItems:Vector.<PreloaderItem>;
		
		private var _currentColumn:int;
		
		public function NavigatorBase() {
			super();
			children = new Vector.<DisplayObject>();
			renderedChildren = new Vector.<DisplayObject>();
			container = new Sprite();
			addChild(container);
		}
		
		private function createMask():void {
			containerMask = new Shape();
			var w:uint = (defaultItemWidth + horizontalGap) * numColumns - horizontalGap;
			var h:uint = (defaultItemHeight + verticalGap) * numRows - verticalGap;
			if (!h) h = 120;
			// если хотим дебажить маску то отрисовываем её красной прозрачной
			if (debugMask) {
				containerMask.graphics.beginFill(0xFF6600, .2);
			} else {
				containerMask.graphics.beginFill(0);
			}
			// если маску нужно немного расширить, так как элементы могут выходить за границы и быть с анимацией
			if (extraPaddingForMask) {
				containerMask.graphics.drawRect(0 + extraPaddingForMask.x, 0 + extraPaddingForMask.y,
					w + extraPaddingForMask.width, h + extraPaddingForMask.height);
			} else {
				containerMask.graphics.drawRect(0, 0, w, h);
			}
			addChild(containerMask);
			if (!debugMask) {
				container.mask = containerMask;
			}
		}
		
		private function renderChild(child:DisplayObject):void {
			child.visible = true;
			if (preload && preloaderItems && preloaderItems.length) {
				var preloaderItem:PreloaderItem = preloaderItems.pop();
				if (container.contains(preloaderItem)) container.removeChild(preloaderItem);
			}
			container.addChild(child);
			renderedChildren.push(child);
		}
		
		private function setChildPosition(child:DisplayObject, index:uint):void {
			const itemsPerPage:uint = numColumns * numRows;
			const page:uint = index / itemsPerPage;
			const column:uint = (index % itemsPerPage) % numColumns;
			const row:uint = (index % itemsPerPage) / numColumns;
			child.x = (defaultItemWidth + horizontalGap) * (page * numColumns + column);
			child.y = (defaultItemHeight + verticalGap) * row;
		}
		
		protected function moveContainerToCurrentIndex():void {
			container.x = -(defaultItemWidth + horizontalGap) * _currentColumn;
			
			// чайлды что за бортом, скрываем
			var child:DisplayObject, page:uint, column:uint;
			const itemsPerPage:uint = numColumns * numRows;
			const len:uint = renderedChildren.length;
			for (var i:uint = 0; i < len; ++i) {
				child = children[i];
				if (!child.visible) continue;
				page = i / itemsPerPage;
				column = (i % numColumns) + page * numColumns;
				if (column >= _currentColumn && column < _currentColumn + numColumns) continue;
				child.visible = false;
			}
		}
		
		//--------------------------------------------------------------------------
		//
		//  Public Functions
		//
		//--------------------------------------------------------------------------
		
		/**
		 * Добавить итем в навигатор.
		 * 
		 * @param child добавляемый итем
		 */
		public function pushChild(child:DisplayObject):void {
			if (!containerMask) createMask();
			setChildPosition(child, children.length);
			children.push(child);
			if (renderedChildren.length < numColumns * numRows) {
				renderChild(child);
			}
			
			dispatchEvent(new Event(FORCE_CHECK_LIMITS));
		}
		
		/**
		 * Убрать итем из навигатора.
		 * 
		 * @param child убираемый итем
		 */
		public function pullChild(child:DisplayObject):void {
			var index:int = children.indexOf(child);
			if (index == -1) return;
			children.splice(index, 1);
			if (renderedChildren.indexOf(child) != -1) {
				renderedChildren.splice(renderedChildren.indexOf(child), 1);
			}
			container.removeChild(child);
			
			var nextChildIndex:int = currentColumn + renderedChildren.length;
			
			if (children.length > nextChildIndex)
				renderChild(children[nextChildIndex]);
			
			for (var i:int = 0; i < children.length; ++i) {
				setChildPosition(children[i], i);
			}
			
			if (index < currentColumn) {
				currentColumn--;
			} else {
				currentColumn = currentColumn;
			}
			
			dispatchEvent(new Event(FORCE_CHECK_LIMITS));
		}
		
		/**
		 * Убрать все итемы из навигатора.
		 */
		public function clear():void {
			var obj:DisplayObject;
			while (renderedChildren.length > 0) {
				obj = renderedChildren.pop();
				if (container.contains(obj)) {
					container.removeChild(obj);
				}
			}
			
			children = new Vector.<DisplayObject>();
			renderedChildren = new Vector.<DisplayObject>();
			
			container.x = container.y = 0;
			_currentColumn = 0;
		}
		
		//--------------------------------------------------------------------------
		//
		//  Getters / Setters
		//
		//--------------------------------------------------------------------------
		
		/**
		 * Текущее положение навигатора.
		 * Можно прибавлять/удалять сразу по несколько, если нужно промотать на несколько итемов сразу.
		 * 
		 * @return текущее положение скроллера навигатора
		 */
		public function get currentColumn():int { return _currentColumn }
		public function set currentColumn(value:int):void {
			// ограничиваем сверху и снизу
			value = value < 0 ? 0 : value > maxIndex ? maxIndex : value;
			_currentColumn = value;
			
			// добавляем в контейнер те чайлды что ещё не на сцене
			var child:DisplayObject, index:uint;
			const itemsPerPage:uint = numColumns * numRows;
			const maxColumn:uint = value + numColumns;
			for (var r:int = 0; r < numRows; ++r) {
				for (var c:uint = value; c < maxColumn; ++c) {
					index = int(c / numColumns) * itemsPerPage + r * numColumns + c % numColumns;
					if (index >= children.length) break;
					child = children[index];
					if (renderedChildren.indexOf(child) == -1) {
						container.addChild(child);
						renderedChildren.push(child);
					}
					child.visible = true;
				}
			}
			
			// перемещаем контейнер к нужной позиции
			moveContainerToCurrentIndex();
		}
		
		/**
		 * Будут ли использоваться крутящиеся прелоадеры пока итемы не заполнили навигатор
		 * 
		 * @return тру - будут, фалсе - не будут
		 */		
		public function get preload():Boolean { return preloaderItems != null }
		public function set preload(value:Boolean):void {
			if (!value || preloaderItems) return;
			
			preloaderItems = new Vector.<PreloaderItem>();
			
			var item:PreloaderItem;
			for (var i:int = 0; i < numColumns; i++) {
				item = new PreloaderItem();
				item.x = i * defaultItemWidth + defaultItemWidth / 2;
				item.y = preloaderY;
				preloaderItems.push(item);
				container.addChild(item);
			}
		}
		
		public function get length():int { return children.length }
		
		/**
		 * Есть ли куда скроллить вперед
		 */
		public function get canScrollForward():Boolean { return _currentColumn < maxIndex }
		
		/**
		 * Есть ли куда скроллить назад
		 */
		public function get canScrollBackward():Boolean { return _currentColumn > 0 }
		
		protected function get maxIndex():uint {
			// если строк больше чем одна, тогда листаем постранично
			if (numRows > 1) {
				return Math.max(0, Math.ceil(children.length / (numColumns * numRows) - 1) * numColumns);
			}
			// иначе листаем до упора
			return Math.max(0, Math.ceil(children.length / numRows - numColumns));
		}
		
	}
	
}
