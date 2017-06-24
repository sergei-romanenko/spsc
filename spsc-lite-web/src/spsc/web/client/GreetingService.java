package spsc.web.client;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;

@RemoteServiceRelativePath("supercompile")
public interface GreetingService extends RemoteService {
	String greetServer(String[] xx);
}
