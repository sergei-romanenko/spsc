package spsc.tests;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

public class All_Tests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tests for SPSC");
		//$JUnit-BEGIN$
//		suite.addTest(new JUnit4TestAdapter(spsc.SmallLanguageParsersTest.class));
//		suite.addTest(new JUnit4TestAdapter(spsc.InterpreterTest.class));
//		suite.addTest(new JUnit4TestAdapter(spsc.LazinessTest.class));
//		suite.addTest(new JUnit4TestAdapter(spsc.SuperCompilerTest.class));
//		suite.addTest(new JUnit4TestAdapter(spsc.TermAlgebraTest.class));
//		suite.addTest(new JUnit4TestAdapter(spsc.SmallLanguageTermAlgebraTest.class));
		suite.addTest(new JUnit4TestAdapter(spsc.tests.MSG_Tests.class));
		//$JUnit-END$
		return suite;
	}

}
