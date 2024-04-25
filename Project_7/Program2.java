// Sotirios Dimitrakoulakos
// [Replace this comment with your own name.]

// [Do necessary modifications of this file.]

package paradis.assignment3;

// [You are welcome to add some import statements.]

import java.util.Arrays;

public class Program2 {
    final static int NUM_WEBPAGES = 100;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    // [You are welcome to add some variables.]

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
        }
    }

    // [Do modify this sequential part of the program.]
    private static WebPage downloadWebPage(WebPage web_page) {
        web_page.download();
        return web_page;
    }

    // [Do modify this sequential part of the program.]
    private static WebPage analyzeWebPage(WebPage web_page) {
        web_page.analyze();
        return web_page;
    }

    // [Do modify this sequential part of the program.]
    private static WebPage categorizeWebPage(WebPage web_page) {
        web_page.categorize();
        return web_page;
    }

    // [You are welcome to modify this method, but it should NOT be parallelized.]
    private static void presentResult(WebPage[] web_pages) {
        for (WebPage i : web_pages) {
            System.out.println(i);
        }
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        // Do the work.
        WebPage[] ready = Arrays.asList(webPages)
            .parallelStream()
            .map(Program2::downloadWebPage)
            .map(Program2::analyzeWebPage)
            .map(Program2::categorizeWebPage)
            .toArray(WebPage[]::new);

        // Stop timing.
        long stop = System.nanoTime();

        // Present the result.
        presentResult(ready);

        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
