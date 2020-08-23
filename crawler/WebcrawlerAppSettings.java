package crawler;

public class WebcrawlerAppSettings {

    public static final int DEFAULT_FRAME_WIDTH = 500;
    public static final int DEFAULT_FRAME_HEIGHT = 600;

    public static final int DEFAULT_TEXT_FIELD_WIDTH = 350;
    public static final int DEFAULT_TEXT_FIELD_HEIGHT = 25;

    private final int frameWidth;
    private final int frameHeight;

    private final int textFieldWidth;
    private final int textFieldHeight;

    public WebcrawlerAppSettings() {
        frameWidth = DEFAULT_FRAME_WIDTH;
        frameHeight = DEFAULT_FRAME_HEIGHT;

        textFieldWidth = DEFAULT_TEXT_FIELD_WIDTH;
        textFieldHeight = DEFAULT_TEXT_FIELD_HEIGHT;
    }

    public int getFrameWidth() {
        return frameWidth;
    }

    public int getFrameHeight() {
        return frameHeight;
    }

    public int getTextFieldWidth() {
        return textFieldWidth;
    }

    public int getTextFieldHeight() {
        return textFieldHeight;
    }
}
