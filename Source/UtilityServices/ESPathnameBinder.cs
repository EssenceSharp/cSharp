/*
 * Copyright (c) 2014, Alan L. Lovejoy
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation are those
 * of the authors and should not be interpreted as representing official policies, 
 * either expressed or implied, of the Essence Sharp Project.
*/
#region Using declarations
using System;
using System.IO;
using System.Collections.Generic;
using Microsoft.Scripting.Hosting;
using EssenceSharp.UtilityServices;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.UtilityServices {

	public class ESPathnameBinder {

		protected DirectoryInfo		defaultSearchPath;
		protected String		defaultExtension	= null;
		protected IList<String>		searchPaths		= new List<String>();

		public ESPathnameBinder() : this(null) {

		}

		public ESPathnameBinder(DirectoryInfo defaultSearchPath) : this(defaultSearchPath, null) {
		}

		public ESPathnameBinder(DirectoryInfo defaultSearchPath, String defaultExtension) {
			this.defaultSearchPath =  defaultSearchPath ?? ESFileUtility.userProfilePath();
			this.defaultExtension = defaultExtension == null ? null : (defaultExtension.Length < 1 ? null: defaultExtension);
			addSearchPathsFrom(new FileInfo(Path.Combine(DefaultSearchPath.FullName, "searchPaths")));
		}

		public DirectoryInfo DefaultSearchPath {
			get { return defaultSearchPath;}
		}

		public bool HasDefaultExtension {
			get { return defaultExtension != null;}

		}

		public String DefaultExtension {
			get { return defaultExtension;}
		}

		public int indexOfSearchPath(String pathname) {
			return searchPaths.IndexOf(pathname);
		}

		public void searchPathAddLast(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			var index = searchPaths.IndexOf(pathname);
			if (index >= 0) searchPaths.RemoveAt(index);
			searchPaths.Add(pathname);
		}

		public void searchPathAddLastIfAbsent(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			if (pathname == DefaultSearchPath.FullName) return;
			var index = searchPaths.IndexOf(pathname);
			if (index >= 0) return;
			searchPaths.Add(pathname);
		}

		public void searchPathAddFirst(String pathname) {
			searchPathInsert(0, pathname);
		}

		public void searchPathInsert(int insertionIndex, String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			var index = searchPaths.IndexOf(pathname);
			if (index >= 0) {
				searchPaths.RemoveAt(index);
				if (index < insertionIndex) insertionIndex--;
			}
			searchPaths.Insert(insertionIndex, pathname);
		}

		public bool searchPathRemove(String pathname) {
			return searchPaths.Remove(pathname);
		}

		public virtual void addSearchPathsFrom(FileInfo searchPathsFile) {
			if (!searchPathsFile.Exists) return;
			using (var stream = searchPathsFile.OpenText()) {
				String searchPath = "";
				do {
					searchPathAddLastIfAbsent(searchPath);
					searchPath = stream.ReadLine();
				} while (searchPath != null);
			}
		}

		public void searchPathsDo(Action<String> enumerator1) {
			foreach (var pathname in searchPaths) enumerator1(pathname);
		}

		public void searchPathsDoUntil(Func<String , bool> enumerator1) {
			foreach (var pathname in searchPaths) if (enumerator1(pathname)) return;
		}

		protected bool exists(String path, bool mustBeDirectory) {
			return (mustBeDirectory && Directory.Exists(path)) || File.Exists(path);
		}

		public bool pathFor(String pathameSuffix, bool mustBeDirectory, out String path) {
			bool mustCheckForExtension = false;
			String suffixWithExtension = pathameSuffix;
			if (HasDefaultExtension) { 
				var extension = Path.GetExtension(pathameSuffix);
				if (extension != DefaultExtension) {
					mustCheckForExtension = true;
					suffixWithExtension = pathameSuffix + DefaultExtension;
				}
			}
			path = pathameSuffix;
			if (exists(pathameSuffix, mustBeDirectory)) return true;
			if (mustCheckForExtension) {
				path = suffixWithExtension;
				if (exists(path, mustBeDirectory)) {
					return true;
				}
			}
			String innerPath = null;
			searchPathsDoUntil(
				pathnamePrefix => {
					innerPath = Path.Combine(pathnamePrefix, pathameSuffix);
					if (exists(innerPath, mustBeDirectory)) return true;
					if (mustCheckForExtension) {
						innerPath = Path.Combine(pathnamePrefix, suffixWithExtension);
						if (exists(innerPath, mustBeDirectory)) return true;
					}
					innerPath = null;
					return false;
				});
			if (innerPath != null) {
				path = innerPath;
				return true;
			}
			path = Path.Combine(DefaultSearchPath.FullName, pathameSuffix);
			if (exists(path, mustBeDirectory)) return true;
			if (mustCheckForExtension) {
				path = Path.Combine(DefaultSearchPath.FullName, suffixWithExtension);
				return (exists(path, mustBeDirectory));
			}
			return false;
		}

		public bool pathFor(String pathameSuffix, out FileInfo path) {
			String pathString;
			if (pathFor(pathameSuffix, false, out pathString)) {
				path = new FileInfo(pathString);
				return true;
			} else {
				path = null;
			}
			return false;
		}

		public bool pathFor(String pathameSuffix, out DirectoryInfo path) {
			String pathString;
			if (pathFor(pathameSuffix, true, out pathString)) {
				path = new DirectoryInfo(pathString);
				return true;
			} else {
				path = null;
			}
			return false;
		}

	}

}
