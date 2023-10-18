public class HelloWorld {
	static final String str = "Hello world!";
    private int a = 1235698;
	private Integer b = 45678;
	public static long  c = 333L;
	private float d = 2.3f;
	private double e = 4.55;

	public static void main(String[] args) {
		System.out.println(str);
	}

	public String concat(String a, String b) {
		if (a == null) {
			return b;
		} else if (b == null) {
			return a;
		}
		this.a = 110;
		int v = count();
		String k = String.valueOf(v + this.a);

		return String.join(",", a, b, this.toString(), k);
	}

	public int count() {
		return 3;
	}

	static {
		c = 100L;
	}
}
