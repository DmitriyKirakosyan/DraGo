/**
 * User: dima
 * Date: 16/02/12
 * Time: 12:49 PM
 */
package core.component.panel {
import core.display.SceneSprite;

public class VerticalScrollablePanel extends ScrollablePanel{
	public function VerticalScrollablePanel(width:Number, height:Number) {
		super(width, height);
	}

	override protected function componentOutside(component:SceneSprite):Boolean {
		return (component.y< 0 || component.y + component.height > panelHeight);
	}

	override protected function showScroll():void {}
	override protected function hideScroll():void {}

}
}
