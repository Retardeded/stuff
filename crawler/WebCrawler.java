package crawler;


import javax.swing.*;
import java.awt.*;

import static crawler.WebParsing.parseTitleFromHtmlCode;


public class WebCrawler extends JFrame {

    private final Webcrawling model;

    private final JLabel urlLabel;
    private final JTextField urlTextField;
    private final JToggleButton runButton;

    private final JLabel threadsLabel;
    private final JTextField threadsTextField;

    private final JLabel depthLabel;
    private final JTextField depthTextField;
    private final JCheckBox depthCheckBox;

    private final JLabel secondsLimitLabel;
    private final JTextField secondsLimitTextField;
    private final JCheckBox secondsLimitCheckBox;

    private final JLabel elapsedSecondsNameLabel;
    private final JLabel elapsedSecondsLabel;

    private final JLabel parsedPagesNumberNameLabel;
    private final JLabel parsedPagesNumberLabel;

    private final JLabel urlTitleLabel;
    private final JTextField exportUrlTextField;
    private final JButton exportButton;

    public WebCrawler() {

        WebcrawlerAppSettings settings = new WebcrawlerAppSettings();
        setFrameLayout();

        this.model = new Webcrawling();

        urlLabel = new JLabel("URL:");
        urlTextField = new JTextField();
        runButton = new JToggleButton("Run");

        threadsLabel = new JLabel("Threads:");
        threadsTextField = new JTextField();

        depthLabel = new JLabel("Depth:");
        depthTextField = new JTextField();
        depthCheckBox = new JCheckBox("Enabled", true);

        secondsLimitLabel = new JLabel("Limit seconds:");
        secondsLimitTextField = new JTextField();
        secondsLimitCheckBox = new JCheckBox("Enabled", true);

        elapsedSecondsNameLabel = new JLabel("Elapsed seconds:");
        elapsedSecondsLabel = new JLabel("0");

        parsedPagesNumberNameLabel = new JLabel("Parsed pages:");
        parsedPagesNumberLabel = new JLabel("0");
        parsedPagesNumberLabel.setName("ParsedLabel");

        urlTitleLabel = new JLabel();
        exportUrlTextField = new JTextField();
        exportButton = new JButton("Export");

        setFrameElements(settings);
        addElementsToFrame();

        setFrame(settings.getFrameWidth(), settings.getFrameHeight());
    }


    private void setFrameLayout() {
        FlowLayout flowLayout = new FlowLayout();
        flowLayout.setAlignment(FlowLayout.LEFT);
        setLayout(flowLayout);
    }

    private void setFrameElements(WebcrawlerAppSettings settings) {
        setUrlTextField(settings.getTextFieldWidth(), settings.getTextFieldHeight());
        setRunButton();

        setThreadsTextField(settings.getTextFieldWidth(), settings.getTextFieldHeight());

        setDepthTextField(settings.getTextFieldWidth(), settings.getTextFieldHeight());
        setDepthCheckBox();

        setSecondsLimitTextField(settings.getTextFieldWidth(), settings.getTextFieldHeight());
        setSecondsLimitCheckBox();

        setUrlTitleLabel(settings.getTextFieldWidth(), settings.getTextFieldHeight());
        setExportUrlTextField(settings.getTextFieldWidth(), settings.getTextFieldHeight());
        setExportButton();
    }

    private void addElementsToFrame() {
        add(urlLabel, BorderLayout.LINE_START);
        add(urlTextField, BorderLayout.CENTER);
        add(runButton, BorderLayout.LINE_END);

        add(threadsLabel, BorderLayout.LINE_START);
        add(threadsTextField, BorderLayout.CENTER);

        add(depthLabel, BorderLayout.LINE_START);
        add(depthTextField, BorderLayout.CENTER);
        add(depthCheckBox, BorderLayout.LINE_END);

        add(secondsLimitLabel, BorderLayout.LINE_START);
        add(secondsLimitTextField, BorderLayout.CENTER);
        add(secondsLimitCheckBox, BorderLayout.LINE_END);

        add(elapsedSecondsNameLabel, BorderLayout.LINE_START);
        add(elapsedSecondsLabel, BorderLayout.CENTER);

        add(parsedPagesNumberNameLabel, BorderLayout.LINE_START);
        add(parsedPagesNumberLabel, BorderLayout.CENTER);

        add(urlTitleLabel, BorderLayout.LINE_START);
        add(exportUrlTextField, BorderLayout.CENTER);
        add(exportButton, BorderLayout.LINE_END);
    }

    private void setUrlTextField(final int width, final int height) {
        urlTextField.setName("UrlTextField");
        urlTextField.setPreferredSize(new Dimension(width, height));
        urlTextField.setVisible(true);
        urlTextField.setText("https://hi.hyperskill.org/");
    }

    private void setRunButton() {
        runButton.setName("RunButton");
        runButton.setVisible(true);

        runButton.addActionListener(ev -> clickRunButton());
    }

    private void setThreadsTextField(final int width, final int height) {
        threadsTextField.setName("ThreadsTextField");
        threadsTextField.setPreferredSize(new Dimension(width, height));
        threadsTextField.setVisible(true);
        threadsTextField.setText("1");
    }

    private void setDepthTextField(final int width, final int height) {
        depthTextField.setName("DepthTextField");
        depthTextField.setPreferredSize(new Dimension(width, height));
        depthTextField.setVisible(true);
        depthTextField.setText("1");
    }

    private void setDepthCheckBox() {
        depthCheckBox.setName("DepthCheckBox");

        depthCheckBox.addChangeListener(ev -> {
            if (depthCheckBox.isSelected()) {
                depthTextField.setEnabled(true);
            } else {
                depthTextField.setEnabled(false);
                model.resetDepthNumber();
                depthTextField.setText(String.valueOf(model.getDepthNumber()));
            }
        });
    }

    private void setSecondsLimitTextField(final int width, final int height) {
        secondsLimitTextField.setName("SecondsTextField");
        secondsLimitTextField.setPreferredSize(new Dimension(width, height));
        secondsLimitTextField.setVisible(true);
        secondsLimitTextField.setText(String.valueOf(model.getSecondsLimit()));
    }

    private void setSecondsLimitCheckBox() {
        secondsLimitCheckBox.setName("SecondsCheckBox");

        secondsLimitCheckBox.addChangeListener(ev -> {
            if (secondsLimitCheckBox.isSelected()) {
                model.setIsTimeLimited(true);
                secondsLimitTextField.setEnabled(true);
            } else {
                model.setIsTimeLimited(false);
                secondsLimitTextField.setEnabled(false);
                model.resetSecondsLimit();
                secondsLimitTextField.setText(String.valueOf(model.getSecondsLimit()));
            }
        });
    }

    private void setUrlTitleLabel(final int width, final int height) {
        urlTitleLabel.setName("TitleLabel");
        urlTitleLabel.setPreferredSize(new Dimension(width, height));
        urlTitleLabel.setVisible(true);
    }

    private void setExportUrlTextField(final int width, final int height) {
        exportUrlTextField.setName("ExportUrlTextField");
        exportUrlTextField.setPreferredSize(new Dimension(width, height));
        exportUrlTextField.setVisible(true);
        exportUrlTextField.setText("D:\\webcrawling.txt");
    }

    private void setExportButton() {
        exportButton.setName("ExportButton");
        exportButton.setVisible(true);

        exportButton.addActionListener(ev -> clickExportButton());
    }

    private void setFrame(final int width, final int height) {
        setTitle("WebCrawler");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
        setSize(width, height);
        setPreferredSize(new Dimension(width, height));
        setVisible(true);
    }

    private void clickRunButton() {
        runButton.setSelected(true);
        elapsedSecondsLabel.setText("0");
        urlTitleLabel.setText(
                parseTitleFromHtmlCode(
                        (urlTextField.getText()))
        );

        model.setCrawlingThreadsNumber(Integer.parseInt(threadsTextField.getText()));
        model.setDepthNumber(Integer.parseInt(depthTextField.getText()));
        if (secondsLimitCheckBox.isSelected()) {
            model.setIsTimeLimited(true);
            model.setSecondsLimit(Integer.parseInt(secondsLimitTextField.getText()));
        } else {
            model.setIsTimeLimited(false);
            secondsLimitTextField.setText(String.valueOf(model.getSecondsLimit()));
        }

        long startMillis = System.currentTimeMillis();
        model.run(urlTextField.getText());
        long endMillis = System.currentTimeMillis();

        int elapsedTime = (int) ((endMillis - startMillis) / 1000L);
        elapsedSecondsLabel.setText(String.valueOf(elapsedTime));

        model.removeUrlsWithoutTitles();
        parsedPagesNumberLabel.setText(String.valueOf(model.getParsedPagesNumber()));
        runButton.setSelected(false);
    }

    private void clickExportButton() {
        model.export(exportUrlTextField.getText());
    }
}
