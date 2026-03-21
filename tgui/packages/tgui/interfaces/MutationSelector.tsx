import {
  Button,
  Collapsible,
  Flex,
  ProgressBar,
  Section,
  Tooltip,
} from 'tgui-core/components';
import { BooleanLike } from 'tgui-core/react';

import { useBackend } from '../backend';
import { Window } from '../layouts';

type Upgrade = {
  name: string;
  type: string;
  desc: string;
  owned: BooleanLike;
};

type MutationBarData = {
  points_available: number;
  points_used: number;
  points_maximum: number;
};

type MutationData = {
  shell_mutations: Upgrade[];
  spur_mutations: Upgrade[];
  veil_mutations: Upgrade[];
};

export const MutationSelector = (_props: any) => {
  const { data } = useBackend<MutationData>();
  const { shell_mutations, spur_mutations, veil_mutations } = data;

  return (
    <Window theme="xeno" width={500} height={600}>
      <Window.Content scrollable>
        <Section title="Mutation Evolution" key="Mutation Evolution">
          <MutationBar />
        </Section>
        <MutationSection category_name="Shell" mutations={shell_mutations} />
        <MutationSection category_name="Spur" mutations={spur_mutations} />
        <MutationSection category_name="Veil" mutations={veil_mutations} />
      </Window.Content>
    </Window>
  );
};

const MutationBar = (_props: any) => {
  const { data } = useBackend<MutationBarData>();
  const { points_available, points_used, points_maximum } = data;

  let tooltipContent = 'You are ready to buy another mutation.';
  if (points_maximum === points_used) {
    tooltipContent = 'You have the maximum amount of mutations!';
  } else if (points_used >= points_available) {
    tooltipContent = "You can't buy another mutation yet...";
  }

  return (
    <Tooltip content={tooltipContent}>
      <Flex mb={1}>
        <Flex.Item grow>
          <ProgressBar color="green" value={points_available / points_maximum}>
            {`${points_available} / ${points_maximum} `}
          </ProgressBar>
        </Flex.Item>
      </Flex>
    </Tooltip>
  );
};

const MutationSection = (props: {
  category_name: string;
  mutations: Upgrade[];
}) => {
  const { act, data } = useBackend<MutationBarData>();
  const { points_available, points_used } = data;

  return (
    <Collapsible title={`${props.category_name} Mutations`}>
      {props.mutations &&
        props.mutations.map((mutation) => (
          <Section
            title={`${mutation.name}`}
            mb={1}
            key={mutation.name}
            buttons={
              <Button
                content={`Buy`}
                key={mutation.name}
                onClick={() => act('purchase', { upgrade_type: mutation.type })}
                disabled={points_used >= points_available || mutation.owned}
                selected={mutation.owned}
              />
            }
          >
            <Flex direction="column-reverse" align={'left'}>
              {mutation.desc}
            </Flex>
          </Section>
        ))}
    </Collapsible>
  );
};
