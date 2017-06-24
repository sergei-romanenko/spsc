package spsc.web.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.DialogBox;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

public class SpscLiteWeb implements EntryPoint {

	private final GreetingServiceAsync greetingService = GWT.create(GreetingService.class);


	public void onModuleLoad() {
		final Button sendButton = new Button("Supercompile");
		final TextArea input = new TextArea();
		input.setCharacterWidth(50);
		input.setVisibleLines(10);

		sendButton.addStyleName("sendButton");

		RootPanel.get("nameFieldContainer").add(input);
		
		final TextBox targetInput = new TextBox();
		RootPanel.get("targetFieldContainer").add(targetInput);
		
		RootPanel.get("sendButtonContainer").add(sendButton);

		// Focus the cursor on the name field when the app loads
		input.setFocus(true);
		input.selectAll();

		// Create the popup dialog box
		final DialogBox dialogBox = new DialogBox();
		dialogBox.setText("Remote Procedure Call");
		dialogBox.setAnimationEnabled(true);
		final Button closeButton = new Button("Close");
		// We can set the id of a widget by accessing its Element
		closeButton.getElement().setId("closeButton");
		final Label textToServerLabel = new Label();
		final HTML serverResponseLabel = new HTML();
		VerticalPanel dialogVPanel = new VerticalPanel();
		dialogVPanel.add(serverResponseLabel);
		dialogVPanel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
		dialogVPanel.add(closeButton);
		dialogBox.setWidget(dialogVPanel);

		closeButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				dialogBox.hide();
				sendButton.setEnabled(true);
				sendButton.setFocus(true);
			}
		});

		class MyHandler implements ClickHandler {
			public void onClick(ClickEvent event) {
				sendNameToServer();
			}


			private void sendNameToServer() {
				sendButton.setEnabled(false);
				String textToServer = input.getText();
				String target = targetInput.getText();
				textToServerLabel.setText(textToServer);
				serverResponseLabel.setText("");
				greetingService.greetServer(new String[]{textToServer, target}, new AsyncCallback<String>() {
					public void onFailure(Throwable caught) {
						caught.printStackTrace();
						dialogBox.setText("Supercompiler Failed");
						serverResponseLabel.addStyleName("serverResponseLabelError");
						serverResponseLabel.setHTML("Incorrect Source Program");
						dialogBox.center();
						closeButton.setFocus(true);
					}

					public void onSuccess(String result) {
						dialogBox.setText("Supercompiler Result");
						serverResponseLabel.removeStyleName("serverResponseLabelError");
						serverResponseLabel.setHTML(result);
						dialogBox.center();
						closeButton.setFocus(true);
					}
				});
			}
		}

		// Add a handler to send the name to the server
		MyHandler handler = new MyHandler();
		sendButton.addClickHandler(handler);
	}
}
