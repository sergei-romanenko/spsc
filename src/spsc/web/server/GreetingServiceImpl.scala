package spsc.web.server

import spsc.web.client.GreetingService
import com.google.gwt.user.server.rpc.RemoteServiceServlet

class GreetingServiceImpl extends RemoteServiceServlet with GreetingService {

	def greetServer(input: Array[String]) = {
		val sc = new SuperCompiler(SParsers.parseProg(input(0)))
		val pt = sc.buildProcessTree(SParsers.parseTerm(input(1)))
		val (resTerm, resProgram) = new ResidualProgramGenerator(pt).result
		"<b>New Goal:</b>" +
			"<pre>" + resTerm.toString + "</pre>" +
			"<b>New Program:</b>" +
			"<pre>" + resProgram.toString + "</pre>"
	}
}
