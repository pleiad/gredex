import { useState, useEffect } from "react";
import {
  Box,
  Paper,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  FormControl,
  InputLabel,
  MenuItem,
  Typography,
  TextField,
  Switch,
  FormControlLabel,
  FormGroup,
  Stack,
} from "@mui/material";
import Select from "@mui/material/Select";
import type { SelectChangeEvent } from "@mui/material/Select";
import { useNavigate, useParams } from "react-router-dom";
import Editor from "./Editor";
//import examples from './examples'; // assuming examples is in its own module
import base_examples from "../assets/examples.json"; // Importing the examples array from a separate module
import Syntax from "./Syntax";
import LoadableContent from "./LoadableContent";
import DerivationTree from "./DerivationTree";
import SentimentVeryDissatisfiedIcon from "@mui/icons-material/SentimentVeryDissatisfied";
import { red } from "@mui/material/colors";
import { MathJax } from "better-react-mathjax";
import { ArrowDownward } from "@mui/icons-material";

const empty = {
  text:
    " -- Select an example from this list to load it in the scratchpad --" + "",
  program: "",
  desc: "",
};
const examples: { text: string; program: string; desc?: string }[] = [
  empty,
  ...base_examples,
];
export default function Main() {
  const { example } = useParams();
  const navigate = useNavigate();

  const defaultIndex = parseInt(example || "0");
  const [program, setProgram] = useState(examples[defaultIndex]?.program || "");
  const [desc, setDesc] = useState(examples[defaultIndex]?.desc || "");
  const [defaultProgram, setDefaultProgram] = useState(defaultIndex);
  const [syntaxOpen, setSyntaxOpen] = useState(false);

  const [loadingI, setLoadingI] = useState(false); // Loading state for program
  const [loadingT, setLoadingT] = useState(false); // Loading state for type derivation
  const [loadingR, setLoadingR] = useState(false); // Loading state for reduction
  const [loadingRE, setLoadingRE] = useState(false); // Loading state for reduction error
  const [intrinsicTerm, setIntrinsicTerm] = useState<string | null>(null);
  const [tree, setTree] = useState<any>(null);
  const [confs, setConfs] = useState<any[]>([]);
  const [reducing, setReducing] = useState(false);
  const [rerror, setRError] = useState<string | null>(null);
  const [finished, setFinished] = useState(false);
  const [fromStep, setFromStep] = useState(0);
  const [newSteps, setNewSteps] = useState(0);
  const [stepSize, setStepSize] = useState(20);
  const [hideEvidences, setHideEvidences] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const index = parseInt(example || "0");
    handleProgramChange(examples[index]?.program || "");
    setDesc(examples[index]?.desc || "");
    setDefaultProgram(index);
  }, [example]);

  const handleProgramChange = (value: string) => {
    setProgram(
      value
        .replace(/\\/g, "λ")
        .replace(/\band\b/g, "∧")
        .replace(/\bor\b/g, "∨")
        .replace(/\binf\b/g, "∞")
        .replace(/\?/g, "★")
        .replace(/\bdyn\b/g, "★")
        .replace(/\bnot\b/g, "¬")
        .replace(/\bneg\b/g, "¬")
    );
  };

  const handleSelectChange = (event: SelectChangeEvent) => {
    const value = event.target.value;
    navigate(`/${value}`);
  };

  const typecheck = async () => {
    if (program && program.trim().length > 0) {
      setLoadingI(true);
      setLoadingT(true);
      setConfs([]);
      setTree(null);
      setError("");
      setRError(null);
      setFromStep(0);
      setFinished(true);
      setNewSteps(0);
      setReducing(false);

      try {
        const response = await fetch("api/typecheck", {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            Accept: "application/json",
          },
          body: JSON.stringify({
            program: program,
            hideEvidences: hideEvidences,
            fromStep: fromStep,
            stepSize: Math.max(stepSize, 1),
          }),
        });

        const result = await response.json();

        if (result.status == "OK") {
          setIntrinsicTerm(result.intrinsicTerm);
          setTree(result.tree);
          setLoadingI(false);
          setLoadingT(false);
        } else {
          setError(result.error);
          setLoadingI(false);
          setLoadingT(false);
          setIntrinsicTerm("");
          //MathJax.Hub.Queue(["Typeset", MathJax.Hub, "errorBox"]);
        }
      } catch (error) {
        alert("Oh no! error");
        console.error(error);
      }
    }
  };

  useEffect(() => {
    setTimeout(() => {
      if ((window as any).MathJax) {
        (window as any).MathJax?.typeset();
      }
    }, 0);
  }, [intrinsicTerm]);

  const reduce = async () => {
    setLoadingR(true);
    setNewSteps(fromStep);

    try {
      const response = await fetch("api/reduce", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "application/json",
        },
        body: JSON.stringify({
          program: program,
          hideEvidences: hideEvidences,
          fromStep: fromStep,
          stepSize: Math.max(stepSize, 1),
        }),
      });

      const result = await response.json();

      if (result.status === "OK") {
        setConfs((prev) => [...prev, ...result.confs.slice(1)]);
        setRError(result.error);
        setFinished(result.finished);
        setFromStep(result.step);
        setReducing(true);
        setLoadingR(false);
      } else {
        setError(result.error);
        setLoadingR(false);
        setLoadingRE(false);
        setLoadingI(false);
        setLoadingT(false);
        setIntrinsicTerm("");
        setTree(null);
      }
    } catch (error) {
      alert("Oh no! error");
      console.error(error);
      setLoadingR(false);
    }
  };

  const confDerivations = confs.map((c, i) => {
    //store={c.store}
    return (
      <div
        key={"reduce" + i}
        style={{ display: newSteps <= i && loadingR ? "none" : "" }}
      >
        <ArrowDownward style={{ marginTop: "8px" }} />
        <DerivationTree
          tree={c.tree}
          key={"dtree" + i}
          shouldUpdate={newSteps <= i}
        />
      </div>
    );
  });

  const handleHideEvidencesChange = (
    event: React.ChangeEvent<HTMLInputElement>
  ) => {
    setHideEvidences(event.target.checked);
  };

  return (
    <Paper sx={{ p: 2 }}>
      <Dialog open={syntaxOpen} onClose={() => setSyntaxOpen(false)} fullWidth>
        <DialogTitle>Syntax</DialogTitle>
        <DialogContent>
          <Syntax showLabels={true} />
        </DialogContent>
      </Dialog>

      <FormControl fullWidth>
        <InputLabel id="demo-simple-select-helper-label">
          Preloaded examples
        </InputLabel>
        <Select
          value={defaultProgram + ""}
          onChange={handleSelectChange}
          label="Preloaded examples"
        >
          {examples.map((e, i) => (
            <MenuItem key={i} value={i}>
              {e.text}
            </MenuItem>
          ))}
        </Select>
      </FormControl>

      <Box color="text.secondary" mt={1}>
        {desc}
      </Box>

      <Box mt={2}>
        <Typography variant="subtitle1">Program</Typography>
        <Editor
          value={program}
          onChange={handleProgramChange}
          errorText={error}
        />
      </Box>

      <Box mt={2}>
        <Stack direction="row" alignItems="center" spacing={2}>
          <Button variant="contained" color="primary" onClick={typecheck}>
            Typecheck
          </Button>
          <FormGroup>
            <FormControlLabel
              control={
                <Switch
                  checked={hideEvidences}
                  onChange={handleHideEvidencesChange}
                />
              }
              label="Hide evidences"
            />
          </FormGroup>
          <Button sx={{ ml: 2 }} onClick={() => navigate("/0")}>
            Clear
          </Button>
          <Button
            sx={{ ml: 2 }}
            onClick={() =>
              handleProgramChange(examples[defaultProgram]?.program || "")
            }
          >
            Reload
          </Button>
          <Button sx={{ ml: 2 }} onClick={() => setSyntaxOpen(true)}>
            View Syntax
          </Button>
        </Stack>
      </Box>

      {intrinsicTerm ? (
        <div>
          <Typography variant="subtitle1">Parsed program</Typography>
          <LoadableContent loading={loadingI}></LoadableContent>
          <div
            id="intrinsicTerm"
            style={{
              textAlign: "center",
              overflowX: "auto",
              overflowY: "hidden",
            }}
          >
            <MathJax>{"$$" + intrinsicTerm + "$$"}</MathJax>
          </div>
        </div>
      ) : null}
      {loadingT || tree || rerror ? (
        <div>
          <Typography variant="subtitle1">
            Type derivation and reduction
          </Typography>
          <LoadableContent loading={loadingT}></LoadableContent>
          <div style={{ textAlign: "center", display: loadingT ? "none" : "" }}>
            <div
              id="derivationTree"
              style={{
                display:
                  (confs.length > 0 && !loadingR) || reducing ? "none" : "",
              }}
            >
              <DerivationTree tree={tree} />
            </div>
            {confs.length <= 0 && !rerror ? (
              <div style={{ marginTop: "16px", display: "flex" }}>
                <div style={{ flexGrow: 1, textAlign: "left" }}>
                  <Button
                    variant="contained"
                    color={"primary"}
                    onClick={reduce}
                  >
                    Reduce!
                  </Button>
                </div>
                <div>
                  <TextField
                    type="number"
                    label="Step size"
                    value={stepSize}
                    size="small"
                    onChange={(e) =>
                      parseInt(e.target.value) > 0 &&
                      setStepSize(parseInt(e.target.value))
                    }
                    sx={{ ml: 2, width: 100 }}
                  />
                </div>
              </div>
            ) : null}

            {confs.length <= 0 ? null : (
              <div id="reduce">{confDerivations}</div>
            )}
            {rerror ? (
              <div>
                <SentimentVeryDissatisfiedIcon
                  sx={{
                    width: 100,
                    height: 100,
                    display: loadingRE ? "none" : "block",
                    color: red[500],
                    margin: "0 auto",
                  }}
                />
                <Box
                  id="reduceError"
                  sx={{
                    color: red[500],
                    fontSize: "120%",
                    display: loadingRE ? "none" : "block",
                  }}
                >
                  {rerror}
                </Box>
              </div>
            ) : null}
            {!finished && !loadingR && !loadingRE ? (
              <div style={{ marginTop: "16px", display: "flex" }}>
                <div style={{ flexGrow: 1, textAlign: "left" }}>
                  <Button variant="contained" color="primary" onClick={reduce}>
                    Reduce more steps!
                  </Button>
                </div>
                <div>
                  <TextField
                    type="number"
                    label="Step size"
                    value={stepSize}
                    size="small"
                    onChange={(e) =>
                      parseInt(e.target.value) > 0 &&
                      setStepSize(parseInt(e.target.value))
                    }
                    sx={{ ml: 2, width: 100 }}
                  />
                </div>
              </div>
            ) : null}

            <LoadableContent loading={loadingR || loadingRE}></LoadableContent>
          </div>
        </div>
      ) : null}
    </Paper>
  );
}
