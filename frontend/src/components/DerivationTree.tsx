import React, { useState, useRef, useEffect, useCallback } from "react";
import { Paper, SvgIcon, useTheme } from "@mui/material";
import MoreVert from "@mui/icons-material/MoreVert";
import { MathJax } from "better-react-mathjax";
import LoadableContent from "./LoadableContent";
import "../derivation.css";

const ExpandIcon = (props: any) => (
  <SvgIcon {...props}>
    <path d="M9.5,13.09L10.91,14.5L6.41,19H10V21H3V14H5V17.59L9.5,13.09M10.91,9.5L9.5,10.91L5,6.41V10H3V3H10V5H6.41L10.91,9.5M14.5,13.09L19,17.59V14H21V21H14V19H17.59L13.09,14.5L14.5,13.09M13.09,9.5L17.59,5H14V3H21V10H19V6.41L14.5,10.91L13.09,9.5Z" />
  </SvgIcon>
);

const CompressIcon = (props: any) => (
  <SvgIcon {...props}>
    <path d="M19.5,3.09L20.91,4.5L16.41,9H20V11H13V4H15V7.59L19.5,3.09M20.91,19.5L19.5,20.91L15,16.41V20H13V13H20V15H16.41L20.91,19.5M4.5,3.09L9,7.59V4H11V11H4V9H7.59L3.09,4.5L4.5,3.09M3.09,19.5L7.59,15H4V13H11V20H9V16.41L4.5,20.91L3.09,19.5Z" />
  </SvgIcon>
);

type Tree = {
  judgments: string[];
  name: string;
  subtrees: Tree[];
  term: string;
};
interface DerivationTreeProps {
  tree: Tree;
  env?: string;
  store?: string;
  substitutionMode?: number;
  shouldUpdate?: boolean;
}

const DerivationTree: React.FC<DerivationTreeProps> = ({
  tree,
  env,
  store,
  substitutionMode,
}) => {
  const theme = useTheme();
  const [open, setOpen] = useState<Record<string, boolean>>({});
  const [allOpen, setAllOpen] = useState(false);
  const [showOptions, setShowOptions] = useState(false);
  const [loadingPremises, setLoadingPremises] = useState(true);
  const [premiseRendered, setPremiseRendered] = useState(false);
  const ref = useRef<HTMLDivElement>(null);

  const expand = useCallback(
    (key: string) => () => {
      setPremiseRendered(true);
      setOpen((prev) => ({ ...prev, [key]: true }));
    },
    []
  );

  const expandAll = useCallback(
    (b: boolean) => () => {
      setPremiseRendered(true);
      setAllOpen(b);
      if (!b) setOpen({});
    },
    []
  );

  const buildDerivationTree = (node: Tree, key: string): React.ReactNode => {
    const premises = node.subtrees.map((p: Tree, i: number) =>
      buildDerivationTree(p, `${key}:${i}`)
    );

    const judgments = node.judgments.map((j: string, i: number) => (
      <div className="judgment" key={`judgment+${i}`}>
        <MathJax>{`$$${j}$$`}</MathJax>
      </div>
    ));

    const isDarkMode = theme.palette.mode === "dark";
    return (
      <div className="infer" key={`infer${key}`}>
        {key <= "0" && !premiseRendered ? null : (
          <div
            className="premise"
            style={{
              display:
                allOpen || premises.length <= 0 || open[key] ? "" : "none",
              borderBottom: `1px solid ${isDarkMode ? "#bbb" : "#ccc"}`,
            }}
          >
            {key <= "0" ? (
              <div>
                <LoadableContent loading={loadingPremises} />
                <div style={{ display: loadingPremises ? "none" : "" }}>
                  {premises}
                  {judgments}
                </div>
              </div>
            ) : (
              <div>
                {premises}
                {judgments}
              </div>
            )}
          </div>
        )}
        <div
          className="premise premise-more"
          style={{
            display:
              allOpen ||
              (premises.length <= 0 && judgments.length <= 0) ||
              open[key]
                ? "none"
                : "",
            borderBottom: `1px solid ${isDarkMode ? "#bbb" : "#ccc"}`,
          }}
          onClick={expand(key)}
        >
          <MoreVert />
        </div>
        <div className="conclusion">
          {node.name && (
            <div className="ruleName">({node.name.toUpperCase()})</div>
          )}
          <MathJax>{"$$" + node.term + "$$"}</MathJax>
        </div>
      </div>
    );
  };

  useEffect(() => {
    if (loadingPremises && ref.current) {
      setLoadingPremises(false); // MathJax handled automatically
    }
  }, [loadingPremises]);

  const dtree = tree ? buildDerivationTree(tree, "0") : null;

  return (
    <Paper
      onMouseEnter={() => setShowOptions(true)}
      onMouseLeave={() => setShowOptions(false)}
      style={{
        position: "relative",
        padding: "4px",
        overflowX: "auto",
        overflowY: "hidden",
      }}
      ref={ref}
      elevation={3}
    >
      <div className="row bottom-xs">
        <div className="col-xs-12 dtree">
          {dtree}
          <div
            style={{
              position: "absolute",
              top: "8px",
              right: "16px",
              width: "24px",
              height: "24px",
            }}
          >
            {showOptions && (
              <div style={{ cursor: "pointer" }}>
                {allOpen ? (
                  <CompressIcon onClick={expandAll(false)} />
                ) : (
                  <ExpandIcon onClick={expandAll(true)} />
                )}
              </div>
            )}
          </div>
        </div>
        <div className="col-xs-12 end-xs envstore">
          <div style={{ clear: "both", overflow: "hidden" }}></div>
          {env && substitutionMode === 0 && (
            <div style={{ textAlign: "right" }}>
              <div>
                <MathJax>{`\( \gamma \): \(${env}\)`}</MathJax>
              </div>
            </div>
          )}
          {store && (
            <div style={{ textAlign: "right" }}>
              <div>
                <MathJax>{`Store: \(${store}\)`}</MathJax>
              </div>
            </div>
          )}
        </div>
      </div>
    </Paper>
  );
};

export default DerivationTree;
