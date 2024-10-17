# Checks for the presence of files with extensions known to be associated with 
# with malicious activities 
check_for_malicious_files <- function(files) {
  # Define the malicious extensions
  malicious_extensions <- c("_exe", "a6p", "ac", "acr", "action", "air", "apk", "app",
                            "applescript", "awk", "bas", "bat", "bin", "cgi", "chm",
                            "cmd", "com", "cpl", "crt", "csh", "dek", "dld", "dll",
                            "dmg", "drv", "ds", "ebm", "elf", "emf", "esh", "exe",
                            "ezs", "fky", "frs", "fxp", "gadget", "gpe", "gpu", "hlp",
                            "hms", "hta", "icd", "iim", "inf", "ins", "inx", "ipa",
                            "ipf", "isp", "isu", "jar", "js", "jse", "jsp", "jsx",
                            "kix", "ksh", "lib", "lnk", "mcr", "mel", "mem", "mpkg",
                            "mpx", "mrc", "ms", "msc", "msi", "msp", "mst", "mxe",
                            "obs", "ocx", "pas", "pcd", "pex", "pif", "pkg", "pl",
                            "plsc", "pm", "prc", "prg", "pvd", "pwc", "py", "pyc",
                            "pyo", "qpx", "rbx", "reg", "rgs", "rox", "rpj", "scar",
                            "scpt", "scr", "script", "sct", "seed", "sh", "shb",
                            "shs", "spr", "sys", "thm", "tlb", "tms", "u3p", "udf",
                            "url", "vb", "vbe", "vbs", "vbscript", "vdo", "vxd",
                            "wcm", "widget", "wmf", "workflow", "wpk", "ws", "wsc",
                            "wsf", "wsh", "xap", "xqt", "zlq")
  
  file_extensions <- tools::file_ext(files)
  if (any(file_extensions %in% malicious_extensions)) {
    return(TRUE)
  }
  
  return(FALSE)
}
