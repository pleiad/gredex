import { Box, Typography } from "@mui/material";
import { red } from "@mui/material/colors";
import AceEditor from "react-ace";

import "ace-builds/src-noconflict/mode-ocaml";
import "ace-builds/src-noconflict/theme-github";
import "ace-builds/src-noconflict/theme-monokai";
import { MathJax } from "better-react-mathjax";

interface EditorProps {
  value: string;
  onChange: (value: string) => void;
  errorText: string | null;
}

export default function Editor({ value, onChange, errorText }: EditorProps) {
  return (
    <Box pt={1}>
      <AceEditor
        mode="ocaml"
        theme="monokai"
        name="ocaml_editor"
        value={value}
        onChange={onChange}
        editorProps={{ $blockScrolling: true }}
        height="200px"
        width="100%"
        fontSize={16}
      />
      {errorText && (
        <Typography color={red[500]} fontSize="1.2rem" id="errorBox">
          <MathJax>{errorText}</MathJax>
        </Typography>
      )}
    </Box>
  );
}
