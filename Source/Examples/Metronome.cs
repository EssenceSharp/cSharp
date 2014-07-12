using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace EssenceSharp.Examples {

	public class Metronome {

		public delegate void TickHandler(Metronome m, EventArgs e);

		public event TickHandler tick;
		protected int interval = 1000;
		protected EventArgs e = null;

		public void Start() {

			while (true) {
				System.Threading.Thread.Sleep(1000);
				if (tick != null) {
					tick(this, e);
				}
			}

		}

	}

}
