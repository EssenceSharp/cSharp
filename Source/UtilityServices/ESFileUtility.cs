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
using System.Text;
using System.IO;
using EssenceSharp.Properties;
#endregion

namespace EssenceSharp.UtilityServices {

	public static class ESFileUtility {
		
		#region Static Variables

		public static readonly String illegalFilenameChars			= @"<>|:*?\/";
		
		private static readonly String userEssenceSharpPathString		= Environment.ExpandEnvironmentVariables(Settings.Default.PathEnvironmentVarName);
		private static readonly String userProfilePathString			= Environment.ExpandEnvironmentVariables("%UserProfile%");
		private static readonly String userDocumentsPathString			= Path.Combine(userProfilePathString, Settings.Default.UserDocumentsPathString);
		private static readonly String userDesktopPathString			= Path.Combine(userProfilePathString, "Desktop");
		private static readonly String defaultCodeDevelopmentPathString		= Path.Combine(userDocumentsPathString, Settings.Default.CodeDevelopmentPathString);
		private static readonly String defaultEssenceSharpPathString		= Path.Combine(defaultCodeDevelopmentPathString, Settings.Default.EssenceSharpPath);
		
		#endregion
			
		public static String UserProfilePathString {
			get {return userProfilePathString;}
		}
			
		public static String UserDocumentsPathString {
			get {return userDocumentsPathString;}
		}
			
		public static String UserDesktopPathString {
			get {return userDesktopPathString;}
		}
			
		public static String DefaultCodeDevelopmentPathString {
			get {return defaultCodeDevelopmentPathString;}
		}
			
		public static String DefaultEssenceSharpPathString {
			get {	var userPath = userEssenceSharpPathString;
				if (userPath.Length > 0 && userPath[0] != '%') return userPath;
				return defaultEssenceSharpPathString;}
		}
			
		public static DirectoryInfo userProfilePath() {
			try {
				return new DirectoryInfo(UserProfilePathString);
			} catch {
				return null;
			}
		}
			
		public static DirectoryInfo userDocumentsPath() {
			try {
				return new DirectoryInfo(UserDocumentsPathString);
			} catch {
				return null;
			}
		}
			
		public static DirectoryInfo userDesktopPath() {
			try {
				return new DirectoryInfo(UserDesktopPathString);
			} catch {
				return null;
			}
		}
			
		public static DirectoryInfo defaultCodeDevelopmentPath() {
			try {
				return new DirectoryInfo(DefaultCodeDevelopmentPathString);
			} catch {
				return null;
			}
		}
			
		public static DirectoryInfo defaultEssenceSharpPath() {
			try {
				return new DirectoryInfo(DefaultEssenceSharpPathString);
			} catch {
				return null;
			}
		}

		public static String filenameWithoutExtensionsFrom(FileSystemInfo declarationFile) {
			var stream = new StringReader(declarationFile.Name);
			var sb = new StringBuilder();
			stream.appendOntoUntil(sb, ch => ch == '.');
			return sb.ToString();
		}

		public static TextReader newReadStream(String filePathString) {
			if (filePathString == null) return null;
			try {
				return newReadStream(new FileInfo(filePathString));
			} catch {
				return null;
			}
		}
		
		public static TextReader newReadStream(DirectoryInfo folder, String localName) {
			return newReadStream(folder, localName, null);
		}
		
		public static TextReader newReadStream(DirectoryInfo folder, String localName, String extension) {
			if (folder == null) return null;
			if (localName == null) return null;
			try {
				String pathString = Path.Combine(folder.FullName, localName);
				if (!String.IsNullOrEmpty(extension)) pathString = Path.ChangeExtension(pathString, extension);
				return newReadStream(new FileInfo(pathString));
			} catch {
				return null;
			}
		}

		public static TextReader newReadStream(FileInfo filePath) {
			try {
				if (filePath == null || !filePath.Exists) return null;
				return filePath.OpenText();
			} catch {
				return null;
			}
		}
		
	}

}
