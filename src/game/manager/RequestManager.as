/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 4:40 PM
 */
package game.manager {
import core.enum.WindowsENUM;
import core.window.WindowManager;

import flash.events.Event;

import game.staticModel.UserState;

public class RequestManager {
	public function RequestManager():void {
		super();
		UserState.instance.addEventListener(Event.CHANGE, onUserStateChange);
	}

	private function onUserStateChange(event:Event):void {
		WindowManager.instance.showWindow(WindowsENUM.REQUEST_WINDOW);
	}
}
}
