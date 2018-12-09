package advent;

import java.util.Arrays;

/**
 * Created by bloat on 09/12/2018.
 */
public class Day92018 {

    public static final int TOP_SCORE = 7202600;
    public static final int NO_PLAYERS = 471;

    private static class Marble {
        private Marble left;
        private Marble right;
        private int val;

        private Marble(int val) {
            this.val = val;
        }

        @Override
        public String toString() {
            return "" + val;
        }
    }

    public static void main(String[] args) {
        Marble zero = new Marble(0);
        zero.left = zero;
        zero.right = zero;

        long[] scores = new long[NO_PLAYERS];
        Arrays.fill(scores, 0L);

        Marble current = zero;

        int player = 0;
        for (int i = 1; i <= TOP_SCORE; i++) {

            if (i % 23 == 0) {
                Marble toRemove = current.left.left.left.left.left.left.left;
                current = toRemove.right;
                toRemove.left.right = toRemove.right;
                toRemove.right.left = toRemove.left;
                scores[player] += i + toRemove.val;
            } else {
                Marble newMarble = new Marble(i);
                newMarble.left = current.right;
                newMarble.right = current.right.right;

                current.right.right = newMarble;
                newMarble.right.left = newMarble;

                current = newMarble;
            }

            player = (player + 1) % NO_PLAYERS;

        }

        System.out.println(Arrays.stream(scores).max().getAsLong());
    }


}
