package crawler;

import org.jetbrains.annotations.NotNull;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WebParsing {

    public static @NotNull
    String parseHtmlCode(final @NotNull String url) {
        String webpageHtmlCode = "";

        try {
            CookieHandler.setDefault(new CookieManager(null, CookiePolicy.ACCEPT_ALL));
            URLConnection urlConnection = new URL(url).openConnection();
            if (urlConnection != null) {
                urlConnection.setRequestProperty("User-Agent",
                        "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:63.0) Gecko/20100101 Firefox/63.0");

                int responseCode = ((HttpURLConnection) urlConnection).getResponseCode();
                if (responseCode == 404
                        || responseCode == 400
                        || urlConnection.getContentType() == null
                        || !urlConnection.getContentType().startsWith("text/html")) {
                    return webpageHtmlCode;
                }

                try (InputStream inputStream =
                             new BufferedInputStream(urlConnection.getInputStream())) {

                    webpageHtmlCode = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
                } catch (IOException exc) {
                    exc.printStackTrace();
                }
            }
        } catch (IOException exc) {
            exc.printStackTrace();
        }

        return webpageHtmlCode;
    }

    public static @NotNull String parseTitle(final @NotNull String url) {
        return parseTitleFromHtmlCode(
                parseHtmlCode(url)
        );
    }

    public static @NotNull String parseTitleFromHtmlCode(final @NotNull String code) {
        Pattern javaPattern = Pattern.compile("(.*?<title>)(.*?)(</title>.*?)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = javaPattern.matcher(code);

        String webpageTitle = "";
        if (matcher.find()) {
            webpageTitle = matcher.group(2);
        }

        return webpageTitle;
    }

    public static @NotNull List<String> parseLinksFromHtmlCode(
            final @NotNull String baseUrl, final @NotNull String code) {
        List<String> urls = new ArrayList<>();

        Pattern patternLinkTag = Pattern.compile("(?i)(<a.*?href=[\"'])(.*?)([\"'].*?>)(.*?)(</a>)");
        Matcher matcherWebpageHtmlCode = patternLinkTag.matcher(code);

        String webpageUrlProtocol = baseUrl.substring(0, baseUrl.indexOf("//"));

        Matcher matcherWebpageUrlBase = Pattern.compile("^.+/").matcher(baseUrl);
        String webpageUrlBase = "";
        if (matcherWebpageUrlBase.find()) {
            webpageUrlBase = matcherWebpageUrlBase.group();
        }

        StringBuilder parsedUrl;
        while (matcherWebpageHtmlCode.find()) {
            parsedUrl = new StringBuilder().append(
                    matcherWebpageHtmlCode.group(2)
            );

            if (parsedUrl.toString().startsWith("#")) {
                continue;
            } else if (!parsedUrl.toString().startsWith("http")) {
                if (parsedUrl.toString().startsWith("//")) {
                    parsedUrl.insert(0, webpageUrlProtocol);
                } else if (parsedUrl.toString().startsWith("/")) {
                    parsedUrl.insert(0, baseUrl);
                } else {
                    parsedUrl.insert(0, webpageUrlBase);
                }
            }

            urls.add(parsedUrl.toString());
        }

        return urls;
    }
}
